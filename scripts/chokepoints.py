#!/usr/bin/env python3
"""Find the modules that slow down the cabal build.

Reads the module graph from the interface files (.hi), and reports:

  * Critical path: Longest chain of modules that GHC must compile one
    after the other;
  * Rebuild cascades: Modules whose change recompiles the most modules;
  * Narrow waists: Modules with many dependencies and many dependents,
    which hold back the parallel build.

With `--mermaid`, prints a Markdown report instead of the tables.

The score is the number of modules a module depends on, multiplied by the number
of modules that depend on it.

The components come from cabal's plan.json and their build-info.json files, so
build with `cabal build all --enable-build-info` first.
"""

import argparse
import graphlib
import json
import os
import re
import subprocess
import sys
from collections import Counter, defaultdict
from collections.abc import Iterable, Sequence
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from itertools import pairwise
from pathlib import Path

type Unit = str
type Row = Sequence[str | int]

INTERFACE = re.compile(r"^interface (\S+)", re.MULTILINE)
USAGE = re.compile(r"\bimport\s+\S+\s+(\S+)\s+[0-9a-f]{32}")
IMPORT = re.compile(
    r"^import\s+(?:(?:\{-#\s*SOURCE\s*#-\}|safe|qualified|splice|quote|\"[^\"]*\")\s+)*"
    r"([A-Z][\w']*(?:\.[A-Z][\w']*)*)",
    re.MULTILINE,
)


@dataclass(frozen=True, order=True)
class Module:
    unit: Unit
    name: str

    def __str__(self) -> str:
        return f"{self.unit}:{self.name}"


type Edge = tuple[Module, Module]


@dataclass(frozen=True)
class Component:
    unit: Unit
    label: str
    dist_dir: Path
    source_dirs: list[Path]
    main: Path | None
    depends: list[Unit]

    def source(self, name: str) -> Path | None:
        if name == "Main":
            return self.main
        stem = name.replace(".", "/")
        found = (d / f"{stem}{e}" for d in self.source_dirs for e in (".hs", ".lhs"))
        return next((f for f in found if f.exists()), None)


@dataclass(frozen=True)
class Interface:
    module: Module
    usages: list[str]


@dataclass(frozen=True)
class Project:
    components: dict[Unit, Component]
    names: dict[Module, str]
    usages: set[Edge]
    imports: set[Edge]


@dataclass(frozen=True)
class Graph:
    deps: dict[Module, set[Module]]
    users: dict[Module, set[Module]]
    ancestors: dict[Module, set[Module]]
    descendants: dict[Module, set[Module]]
    finish: dict[Module, int]
    chokepoints: set[Module]
    path: list[Module]


@dataclass(frozen=True)
class Section:
    title: str
    header: Row
    modules: list[Module]
    rows: list[Row]


def read_components(builddir: Path) -> dict[Unit, Component]:
    plan = json.loads((builddir / "cache" / "plan.json").read_text())
    components: dict[Unit, Component] = {}
    for unit in plan["install-plan"]:
        if unit.get("style") != "local" or unit["pkg-src"]["type"] != "local":
            continue
        dist_dir = Path(unit["dist-dir"])
        if not dist_dir.exists():
            continue
        build_info = Path(unit["build-info"])
        if not build_info.exists():
            sys.exit(
                f"chokepoints: {build_info} is missing; "
                "build with `cabal build all --enable-build-info`"
            )
        [info] = json.loads(build_info.read_text())["components"]
        source_dirs = [Path(info["src-dir"]) / d for d in info["hs-src-dirs"]]
        source_dirs += sorted(d for d in dist_dir.rglob("autogen") if d.is_dir())
        mains = (d / f for f in info["src-files"] for d in source_dirs)
        components[unit["id"]] = Component(
            unit=unit["id"],
            label=f"{unit['pkg-name']}:{unit['component-name']}",
            dist_dir=dist_dir,
            source_dirs=source_dirs,
            main=next((f for f in mains if f.exists()), None),
            depends=unit.get("depends", []),
        )
    return components


def read_interface(ghc: str, unit: Unit, path: Path) -> Interface:
    cmd = [ghc, "-package-env", "-", "--show-iface", str(path)]
    try:
        out = subprocess.run(cmd, check=True, capture_output=True, text=True).stdout
    except FileNotFoundError:
        sys.exit(f"chokepoints: {ghc} not found; give the GHC that built the project")
    except subprocess.CalledProcessError as e:
        reason = e.stderr.strip().splitlines()[-1:] or ["no error output"]
        sys.exit(
            f"chokepoints: {ghc} --show-iface {path} failed: {reason[0]}\n"
            "Give the GHC that built the project with --ghc or $GHC."
        )
    if not (name := INTERFACE.search(out)):
        sys.exit(f"chokepoints: cannot read {path}")
    return Interface(Module(unit, name.group(1)), USAGE.findall(out))


def read_interfaces(ghc: str, components: Iterable[Component]) -> list[Interface]:
    found = [(c.unit, p) for c in components for p in sorted(c.dist_dir.rglob("*.hi"))]
    with ThreadPoolExecutor(os.cpu_count()) as pool:
        return list(pool.map(lambda f: read_interface(ghc, *f), found))


def source_imports(path: Path) -> set[str]:
    return set(IMPORT.findall(path.read_text(encoding="utf-8", errors="replace")))


def local_depends(components: dict[Unit, Component]) -> dict[Unit, list[Unit]]:
    return {
        u: sorted(d for d in c.depends if d in components)
        for u, c in components.items()
    }


def read_project(ghc: str, builddir: Path) -> tuple[Project, int]:
    components = read_components(builddir)
    interfaces = read_interfaces(ghc, components.values())
    sources = {
        i.module: source
        for i in interfaces
        if (source := components[i.module.unit].source(i.module.name))
    }
    kept = [i for i in interfaces if i.module in sources]
    depends = local_depends(components)
    closure: dict[Unit, set[Unit]] = {}
    for u in topo_order(depends):
        closure[u] = set().union(*(closure[d] | {d} for d in depends[u]))
    usages: set[Edge] = set()
    imports: set[Edge] = set()
    for i in kept:
        n = i.module
        used: set[Module] = set()
        for usage in i.usages:
            home, _, name = usage.rpartition(":")
            used.update(Module(u, name) for u in ([home] if home else closure[n.unit]))
        used = {u for u in used if u in sources and u != n}
        imported = source_imports(sources[n])
        usages |= {(n, u) for u in used}
        imports |= {(n, u) for u in used if u.name in imported}
    count = Counter(i.module.name for i in kept)
    names = {
        i.module: i.module.name
        if count[i.module.name] == 1
        else f"{i.module.name} ({components[i.module.unit].label})"
        for i in kept
    }
    project = Project(components, names, usages, imports)
    return project, len(interfaces) - len(kept)


def adjacency(
    nodes: Iterable[Module], edges: Iterable[Edge]
) -> tuple[dict[Module, set[Module]], dict[Module, set[Module]]]:
    deps: dict[Module, set[Module]] = {n: set() for n in nodes}
    users: dict[Module, set[Module]] = {n: set() for n in nodes}
    for user, used in edges:
        deps[user].add(used)
        users[used].add(user)
    return deps, users


def topo_order[T](deps: dict[T, Iterable[T]]) -> list[T]:
    try:
        return list(graphlib.TopologicalSorter(deps).static_order())
    except graphlib.CycleError:
        sys.exit("chokepoints: the module graph has a cycle")


def reach(
    order: Iterable[Module], next_to: dict[Module, set[Module]]
) -> dict[Module, set[Module]]:
    reached: dict[Module, set[Module]] = {}
    for n in order:
        reached[n] = set().union(*(reached[d] | {d} for d in next_to[n]))
    return reached


def schedule(
    p: Project, used: dict[Module, set[Module]]
) -> tuple[dict[Module, int], set[Module], list[Module]]:
    by_unit: defaultdict[Unit, list[Module]] = defaultdict(list)
    for n in sorted(used):
        by_unit[n.unit].append(n)
    waits = local_depends(p.components)
    inside = {n: {d for d in used[n] if d.unit == n.unit} for n in used}
    within = {u: topo_order({n: inside[n] for n in by_unit[u]}) for u in waits}
    units = topo_order(waits)

    type Start = tuple[int, Module | None]

    def later(a: Start, b: Start) -> Start:
        return b if b[0] > a[0] else a

    finish: dict[Module, int] = {}
    via: dict[Module, Module | None] = {}
    done: dict[Unit, Start] = {}
    for u in units:
        ready: Start = (0, None)
        for d in waits[u]:
            ready = later(ready, done[d])
        for n in within[u]:
            start = ready
            for d in sorted(inside[n]):
                start = later(start, (finish[d], d))
            finish[n], via[n] = start[0] + 1, start[1]
        done[u] = ready
        for n in by_unit[u]:
            done[u] = later(done[u], (finish[n], n))

    critical = max(finish.values(), default=0)
    users_of = {n: {x for x in used if n in inside[x]} for n in used}
    dependents: defaultdict[Unit, list[Unit]] = defaultdict(list)
    for u, ds in waits.items():
        for d in ds:
            dependents[d].append(u)
    latest: dict[Module, int] = {}
    deadline: dict[Unit, int] = {}
    for u in reversed(units):
        deadline[u] = min(
            (
                min((latest[n] - 1 for n in by_unit[w]), default=deadline[w])
                for w in dependents[u]
            ),
            default=critical,
        )
        for n in reversed(within[u]):
            latest[n] = min((latest[x] - 1 for x in users_of[n]), default=critical)
            latest[n] = min(latest[n], deadline[u])

    path: list[Module] = []
    cursor = max(sorted(finish), key=lambda n: finish[n], default=None)
    while cursor is not None:
        path.append(cursor)
        cursor = via[cursor]
    path.reverse()
    return finish, {n for n in finish if latest[n] == finish[n]}, path


def analyse(p: Project) -> Graph:
    nodes = sorted(p.names)
    used, users_of = adjacency(nodes, p.usages)
    order = topo_order(used)
    finish, chokepoints, path = schedule(p, used)
    deps, users = adjacency(nodes, p.imports)
    return Graph(
        deps=deps,
        users=users,
        ancestors=reach(order, used),
        descendants=reach(reversed(order), users_of),
        finish=finish,
        chokepoints=chokepoints,
        path=path,
    )


def score(g: Graph, n: Module) -> int:
    return len(g.ancestors[n]) * len(g.descendants[n])


def yes_no(flag: bool) -> str:
    return "yes" if flag else "no"


def sections(p: Project, g: Graph, top: int) -> dict[str, Section]:
    names = p.names

    def label(n: Module) -> str:
        return p.components[n.unit].label

    ranked = sorted(
        (n for n in names if score(g, n)), key=lambda n: (-score(g, n), names[n])
    )
    blocking = [n for n in ranked if n in g.chokepoints][:top]
    cascades = sorted(names, key=lambda n: (-len(g.descendants[n]), names[n]))[:top]
    waists = ranked[:top]
    return {
        "blocking": Section(
            "Narrow waists that are chokepoints",
            ("module", "component", "deps", "dependents", "score"),
            blocking,
            [
                (
                    names[n],
                    label(n),
                    len(g.ancestors[n]),
                    len(g.descendants[n]),
                    score(g, n),
                )
                for n in blocking
            ],
        ),
        "path": Section(
            "Critical path (compiled one after the other)",
            ("module", "component", "depth"),
            g.path,
            [(names[n], label(n), g.finish[n]) for n in g.path],
        ),
        "cascades": Section(
            "Rebuild cascades (modules recompiled when the module changes)",
            ("module", "dependents", "chokepoint"),
            cascades,
            [
                (names[n], len(g.descendants[n]), yes_no(n in g.chokepoints))
                for n in cascades
            ],
        ),
        "waists": Section(
            "Narrow waists (dependencies x dependents)",
            ("module", "deps", "dependents", "score", "chokepoint"),
            waists,
            [
                (
                    names[n],
                    len(g.ancestors[n]),
                    len(g.descendants[n]),
                    score(g, n),
                    yes_no(n in g.chokepoints),
                )
                for n in waists
            ],
        ),
    }


def table(section: Section) -> None:
    header, rows = section.header, section.rows
    widths = [max(len(str(r[i])) for r in [header, *rows]) for i in range(len(header))]
    fmt = "  ".join(
        f"{{:<{w}}}" if i == 0 else f"{{:>{w}}}" for i, w in enumerate(widths)
    )
    print(f"\n== {section.title} ==")
    print(fmt.format(*header))
    for r in rows:
        print(fmt.format(*r))


def markdown_table(section: Section) -> list[str]:
    align = [
        "---:" if all(isinstance(row[i], int) for row in section.rows) else "---"
        for i in range(len(section.header))
    ]
    return [
        f"| {' | '.join(str(c) for c in row)} |"
        for row in [section.header, align, *section.rows]
    ]


def bar_chart(section: Section, column: str) -> list[str]:
    if not section.rows:
        return ["No modules."]
    i = section.header.index(column)
    labels = ", ".join(f'"{row[0]}"' for row in section.rows)
    values = [int(row[i]) for row in section.rows]
    return [
        "```mermaid",
        "xychart-beta horizontal",
        f'  title "{section.title}"',
        f"  x-axis [{labels}]",
        f'  y-axis "{column}" 0 --> {max(values)}',
        f"  bar [{', '.join(map(str, values))}]",
        "```",
    ]


def neighbourhood(n: Module, names: dict[Module, str], g: Graph) -> list[str]:
    lines = [
        "```mermaid",
        "flowchart LR",
        f'  {n}["{names[n]}"]',
        "  classDef focus fill:#fdd,stroke:#c00,color:#000",
        f"  class {n} focus",
    ]
    for title, side, members, arrow in (
        ("depends on", "deps", g.deps[n], f"deps --> {n}"),
        ("used by", "users", g.users[n], f"{n} --> users"),
    ):
        if not members:
            continue
        listed = "<br/>".join(sorted(names[m] for m in members))
        lines.append(f'  {side}["<b>{title}</b><br/>{listed}"]')
        lines.append(f"  {arrow}")
    lines.append("```")
    return lines


def report(p: Project, g: Graph, s: dict[str, Section]) -> str:
    names = p.names
    lines = ["# Build chokepoints", "", f"## {s['blocking'].title}", ""]
    lines += markdown_table(s["blocking"])
    lines += [
        "",
        "## Critical path",
        "",
        f"{len(g.path)} modules, compiled one after the other. A dotted arrow"
        " waits for the whole component to be built.",
        "",
        "```mermaid",
        "flowchart TB",
    ]
    lines += [f'  {n}["{names[n]}"]' for n in g.path]
    lines += [
        f"  {a} --> {b}"
        if a.unit == b.unit
        else f"  {a} -.->|{p.components[a.unit].label} built| {b}"
        for a, b in pairwise(g.path)
    ]
    lines += ["```", "", "## Rebuild cascades", ""]
    lines += bar_chart(s["cascades"], "dependents")
    lines += ["", "## Narrow waists", ""]
    lines += bar_chart(s["waists"], "score")
    for n in s["waists"].modules:
        lines += [
            "",
            f"### {names[n]}",
            "",
            f"{len(g.ancestors[n])} dependencies, {len(g.descendants[n])} dependents"
            f" (direct: {len(g.deps[n])} and {len(g.users[n])})"
            f"{', chokepoint' if n in g.chokepoints else ''}.",
            "",
        ]
        lines += neighbourhood(n, names, g)
    return "\n".join(lines)


def positive(value: str) -> int:
    if (n := int(value)) < 1:
        raise argparse.ArgumentTypeError(f"{value} is not a positive number")
    return n


def main() -> None:
    ap = argparse.ArgumentParser(description=(__doc__ or "").split("\n")[0])
    ap.add_argument(
        "--top", type=positive, default=15, help="rows per table (default 15)"
    )
    ap.add_argument(
        "--builddir",
        type=Path,
        default=Path("dist-newstyle"),
        help="cabal build directory (default dist-newstyle)",
    )
    ap.add_argument(
        "--ghc",
        default=os.environ.get("GHC", "ghc"),
        help="GHC that wrote the interface files (default $GHC or ghc)",
    )
    ap.add_argument(
        "--mermaid",
        action="store_true",
        help="print a Markdown report with Mermaid diagrams instead of the tables",
    )
    args = ap.parse_args()

    p, dropped = read_project(args.ghc, args.builddir)
    g = analyse(p)
    s = sections(p, g, args.top)

    if args.mermaid:
        print(report(p, g, s))
        return

    print(f"Modules: {len(p.names)} ({dropped} without a source file ignored)")
    print(f"Critical path: {len(g.path)} modules")
    if g.path:
        print(f"Parallelism bound: {len(p.names) / len(g.path):.1f} cores")
    for key in ("blocking", "path", "cascades", "waists"):
        table(s[key])


if __name__ == "__main__":
    main()
