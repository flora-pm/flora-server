module Flora.TemplateSpec where

import Data.Text.Display (display)
import Distribution.Compiler (CompilerFlavor (GHC))
import Distribution.System (Arch (I386, X86_64), OS (FreeBSD, Linux, Windows))
import Distribution.Types.Condition (Condition (..))
import Distribution.Types.ConfVar (ConfVar (..))
import Distribution.Types.Flag (mkFlagName)
import Distribution.Version qualified as Version
import RequireCallStack

import Distribution.Orphans.ConfVar ()
import Flora.Model.Package.Types
import Flora.TestUtils
import FloraWeb.Links qualified as Links

spec :: RequireCallStack => TestEff TestTree
spec =
  testThese
    "templates"
    [ testThis "Generate a link to a package + version" testGenerateVersionedPackageLink
    , testThis "Generate a link to a namespace" testGenerateNamespaceLink
    , testThis "Render cabal conditions" testRenderCondition
    ]

testGenerateVersionedPackageLink :: RequireCallStack => TestEff ()
testGenerateVersionedPackageLink = do
  let namespace = Namespace "local-hackage"
  let packageName = PackageName "base"
  let version = Version.mkVersion [4, 16, 1, 0]
  let generatedLink = Links.renderLink $ Links.packageVersionLink namespace packageName version
  assertEqual_ "packages/@local-hackage/base/4.16.1.0" generatedLink

testGenerateNamespaceLink :: RequireCallStack => TestEff ()
testGenerateNamespaceLink = do
  let namespace = Namespace "local-hackage"
  let generatedLink = Links.renderLink $ Links.namespaceLink namespace 2
  assertEqual_ "packages/@local-hackage?page=2" generatedLink

testRenderCondition :: RequireCallStack => TestEff ()
testRenderCondition = do
  let flag = Var . PackageFlag . mkFlagName
  assertEqual_
    "flag(pkg-config) && !os(windows) && !arch(i386)"
    (display (CAnd (flag "pkg-config") (CAnd (CNot (Var (OS Windows))) (CNot (Var (Arch I386))))))
  assertEqual_
    "(os(linux) || os(freebsd)) && flag(journald) && (flag(metal) || flag(gpu))"
    ( display
        ( CAnd
            (COr (Var (OS Linux)) (Var (OS FreeBSD)))
            (CAnd (flag "journald") (COr (flag "metal") (flag "gpu")))
        )
    )
  assertEqual_
    "flag(a) || (flag(b) && flag(c)) || flag(d)"
    (display (COr (COr (flag "a") (CAnd (flag "b") (flag "c"))) (flag "d")))
  assertEqual_
    "flag(static) && !(os(windows) && arch(x86_64)) && !flag(openssl-pkgconfig)"
    ( display
        ( CAnd
            (flag "static")
            (CAnd (CNot (CAnd (Var (OS Windows)) (Var (Arch X86_64)))) (CNot (flag "openssl-pkgconfig")))
        )
    )
  assertEqual_
    "!impl(ghc >=9.6) || impl(ghc)"
    ( display
        ( COr
            (CNot (Var (Impl GHC (Version.orLaterVersion (Version.mkVersion [9, 6])))))
            (Var (Impl GHC Version.anyVersion))
        )
    )
  assertEqual_ "true" (display (Lit @ConfVar True))
