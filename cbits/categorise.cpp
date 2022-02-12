
#include "souffle/CompiledSouffle.h"

extern "C" {
}

namespace souffle {
static const RamDomain RAM_BIT_SHIFT_MASK = RAM_DOMAIN_SIZE - 1;
struct t_btree_iii__0_1_2__111 {
static constexpr Relation::arity_type Arity = 3;
using t_tuple = Tuple<RamDomain, 3>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1 : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0])) ? 1 :((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])) ? -1 : (ramBitCast<RamSigned>(a[1]) > ramBitCast<RamSigned>(b[1])) ? 1 :((ramBitCast<RamSigned>(a[2]) < ramBitCast<RamSigned>(b[2])) ? -1 : (ramBitCast<RamSigned>(a[2]) > ramBitCast<RamSigned>(b[2])) ? 1 :(0)));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0]))|| (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0])) && ((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1]))|| (ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1])) && ((ramBitCast<RamSigned>(a[2]) < ramBitCast<RamSigned>(b[2]))));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0]))&&(ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1]))&&(ramBitCast<RamSigned>(a[2]) == ramBitCast<RamSigned>(b[2]));
 }
};
using t_ind_0 = btree_set<t_tuple,t_comparator_0>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0_lower;
t_ind_0::operation_hints hints_0_upper;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0_lower)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[3];
std::copy(ramDomain, ramDomain + 3, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1,RamDomain a2) {
RamDomain data[3] = {a0,a1,a2};
return insert(data);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0_lower);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0_lower);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> lowerUpperRange_000(const t_tuple& /* lower */, const t_tuple& /* upper */, context& /* h */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_000(const t_tuple& /* lower */, const t_tuple& /* upper */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_111(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp == 0) {
    auto pos = ind_0.find(lower, h.hints_0_lower);
    auto fin = ind_0.end();
    if (pos != fin) {fin = pos; ++fin;}
    return make_range(pos, fin);
}
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_111(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_111(lower,upper,h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printStatistics(std::ostream& o) const {
o << " arity 3 direct b-tree index 0 lex-order [0,1,2]\n";
ind_0.printStats(o);
}
};
struct t_btree_ii__0_1__11__10 {
static constexpr Relation::arity_type Arity = 2;
using t_tuple = Tuple<RamDomain, 2>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1 : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0])) ? 1 :((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])) ? -1 : (ramBitCast<RamSigned>(a[1]) > ramBitCast<RamSigned>(b[1])) ? 1 :(0));
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0]))|| (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0])) && ((ramBitCast<RamSigned>(a[1]) < ramBitCast<RamSigned>(b[1])));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0]))&&(ramBitCast<RamSigned>(a[1]) == ramBitCast<RamSigned>(b[1]));
 }
};
using t_ind_0 = btree_set<t_tuple,t_comparator_0>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0_lower;
t_ind_0::operation_hints hints_0_upper;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0_lower)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[2];
std::copy(ramDomain, ramDomain + 2, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0,RamDomain a1) {
RamDomain data[2] = {a0,a1};
return insert(data);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0_lower);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0_lower);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> lowerUpperRange_00(const t_tuple& /* lower */, const t_tuple& /* upper */, context& /* h */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_00(const t_tuple& /* lower */, const t_tuple& /* upper */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp == 0) {
    auto pos = ind_0.find(lower, h.hints_0_lower);
    auto fin = ind_0.end();
    if (pos != fin) {fin = pos; ++fin;}
    return make_range(pos, fin);
}
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_11(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_11(lower,upper,h);
}
range<t_ind_0::iterator> lowerUpperRange_10(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_10(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_10(lower,upper,h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printStatistics(std::ostream& o) const {
o << " arity 2 direct b-tree index 0 lex-order [0,1]\n";
ind_0.printStats(o);
}
};
struct t_btree_i__0__1 {
static constexpr Relation::arity_type Arity = 1;
using t_tuple = Tuple<RamDomain, 1>;
struct t_comparator_0{
 int operator()(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0])) ? -1 : (ramBitCast<RamSigned>(a[0]) > ramBitCast<RamSigned>(b[0])) ? 1 :(0);
 }
bool less(const t_tuple& a, const t_tuple& b) const {
  return (ramBitCast<RamSigned>(a[0]) < ramBitCast<RamSigned>(b[0]));
 }
bool equal(const t_tuple& a, const t_tuple& b) const {
return (ramBitCast<RamSigned>(a[0]) == ramBitCast<RamSigned>(b[0]));
 }
};
using t_ind_0 = btree_set<t_tuple,t_comparator_0>;
t_ind_0 ind_0;
using iterator = t_ind_0::iterator;
struct context {
t_ind_0::operation_hints hints_0_lower;
t_ind_0::operation_hints hints_0_upper;
};
context createContext() { return context(); }
bool insert(const t_tuple& t) {
context h;
return insert(t, h);
}
bool insert(const t_tuple& t, context& h) {
if (ind_0.insert(t, h.hints_0_lower)) {
return true;
} else return false;
}
bool insert(const RamDomain* ramDomain) {
RamDomain data[1];
std::copy(ramDomain, ramDomain + 1, data);
const t_tuple& tuple = reinterpret_cast<const t_tuple&>(data);
context h;
return insert(tuple, h);
}
bool insert(RamDomain a0) {
RamDomain data[1] = {a0};
return insert(data);
}
bool contains(const t_tuple& t, context& h) const {
return ind_0.contains(t, h.hints_0_lower);
}
bool contains(const t_tuple& t) const {
context h;
return contains(t, h);
}
std::size_t size() const {
return ind_0.size();
}
iterator find(const t_tuple& t, context& h) const {
return ind_0.find(t, h.hints_0_lower);
}
iterator find(const t_tuple& t) const {
context h;
return find(t, h);
}
range<iterator> lowerUpperRange_0(const t_tuple& /* lower */, const t_tuple& /* upper */, context& /* h */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<iterator> lowerUpperRange_0(const t_tuple& /* lower */, const t_tuple& /* upper */) const {
return range<iterator>(ind_0.begin(),ind_0.end());
}
range<t_ind_0::iterator> lowerUpperRange_1(const t_tuple& lower, const t_tuple& upper, context& h) const {
t_comparator_0 comparator;
int cmp = comparator(lower, upper);
if (cmp == 0) {
    auto pos = ind_0.find(lower, h.hints_0_lower);
    auto fin = ind_0.end();
    if (pos != fin) {fin = pos; ++fin;}
    return make_range(pos, fin);
}
if (cmp > 0) {
    return make_range(ind_0.end(), ind_0.end());
}
return make_range(ind_0.lower_bound(lower, h.hints_0_lower), ind_0.upper_bound(upper, h.hints_0_upper));
}
range<t_ind_0::iterator> lowerUpperRange_1(const t_tuple& lower, const t_tuple& upper) const {
context h;
return lowerUpperRange_1(lower,upper,h);
}
bool empty() const {
return ind_0.empty();
}
std::vector<range<iterator>> partition() const {
return ind_0.getChunks(400);
}
void purge() {
ind_0.clear();
}
iterator begin() const {
return ind_0.begin();
}
iterator end() const {
return ind_0.end();
}
void printStatistics(std::ostream& o) const {
o << " arity 1 direct b-tree index 0 lex-order [0]\n";
ind_0.printStats(o);
}
};

class Sf_categorise : public SouffleProgram {
private:
static inline std::string substr_wrapper(const std::string& str, std::size_t idx, std::size_t len) {
   std::string result; 
   try { result = str.substr(idx,len); } catch(...) { 
     std::cerr << "warning: wrong index position provided by substr(\"";
     std::cerr << str << "\"," << (int32_t)idx << "," << (int32_t)len << ") functor.\n";
   } return result;
}
public:
// -- initialize symbol table --
SymbolTable symTable{
	R"_(algorithms)_",
	R"_(Algorithms)_",
	R"_(Algorithms implemented in Haskell, like sorting, searching)_",
	R"_(bioinformatics)_",
	R"_(Bioinformatics)_",
	R"_(Tooling, algorithms and techniques for the domain of Bioinformatics)_",
	R"_(command-line)_",
	R"_(CLI & TUI tooling)_",
	R"_(Libraries to develope command-line interfaces)_",
	R"_(compilers-interpreters)_",
	R"_(Compilers and Interpreters)_",
	R"_(Tooling to create compilers and interpreters)_",
	R"_(cryptography)_",
	R"_(Cryptography)_",
	R"_(Algorithms for encrypting and hashing data)_",
	R"_(data-structures)_",
	R"_(Data Structures)_",
	R"_(Data structures, whether they are purely functional or mutable, there's always one for you)_",
	R"_(databases)_",
	R"_(Databases)_",
	R"_(Database drivers and interfaces)_",
	R"_(development)_",
	R"_(Development)_",
	R"_(Development helpers, integration with other languages)_",
	R"_(distributed)_",
	R"_(Distributed Systems & Computation)_",
	R"_(Tooling and techniques for writing distributed systems)_",
	R"_(ffi)_",
	R"_(FFI)_",
	R"_(Tooling to work with the Foreign Function Interface)_",
	R"_(game-dev)_",
	R"_(Game development)_",
	R"_(Libraries used for game development)_",
	R"_(generics)_",
	R"_(Generics)_",
	R"_(Tooling to work with Haskell's Generics)_",
	R"_(prelude)_",
	R"_(Prelude)_",
	R"_(Libraries that provide default imports)_",
	R"_(language)_",
	R"_(Language)_",
	R"_(Tooling for interfacing with other programming languages from Haskell programs)_",
	R"_(natural-language)_",
	R"_(Natural Language)_",
	R"_(Tooling to working with natural languages)_",
	R"_(network)_",
	R"_(Network Development)_",
	R"_(Connection pools, DNS, HTTP, API clients and network protocols)_",
	R"_(maths)_",
	R"_(Mathematics)_",
	R"_(Numerical and Mathematical packages)_",
	R"_(parser-implementations)_",
	R"_(Parser Implementations)_",
	R"_(Parsing data formats)_",
	R"_(parsers)_",
	R"_(Parsers)_",
	R"_(Libraries to ingest and parse data)_",
	R"_(parsing)_",
	R"_(Parsing)_",
	R"_(Parser generators, combinators and tools to help with parsing)_",
	R"_(system)_",
	R"_(System)_",
	R"_(Programming and communicating with the Operating System)_",
	R"_(testing)_",
	R"_(Testing)_",
	R"_(Test frameworks)_",
	R"_(text)_",
	R"_(Text)_",
	R"_(Working with textual data and algorithms)_",
	R"_(web)_",
	R"_(Web Development)_",
	R"_(Programming for the web)_",
	R"_(xml)_",
	R"_(XML)_",
	R"_(Libraries to consume and produce XML documents)_",
	R"_(Algorithm)_",
	R"_(Crypto)_",
	R"_(CLI)_",
	R"_(CLI & TUI Development)_",
	R"_(TUI)_",
	R"_(Command Line)_",
	R"_(CommandLine)_",
	R"_(Numeric)_",
	R"_(Numerical)_",
	R"_(Numerics)_",
	R"_(Arithmetic)_",
	R"_(Number Theory)_",
	R"_(Math)_",
	R"_(Maths)_",
	R"_(Parser Builder)_",
	R"_(Parser Combinators)_",
	R"_(Parser)_",
	R"_(ParserCombinators)_",
	R"_(Network)_",
	R"_(Data Network)_",
	R"_(Network APIs)_",
	R"_(Network Control)_",
	R"_(NetworkAPI)_",
	R"_(NetworkAPIs)_",
	R"_(Networking)_",
};// -- initialize record table --
SpecializedRecordTable<0> recordTable{};
// -- Table: flora_category
Own<t_btree_iii__0_1_2__111> rel_1_flora_category = mk<t_btree_iii__0_1_2__111>();
souffle::RelationWrapper<t_btree_iii__0_1_2__111> wrapper_rel_1_flora_category;
// -- Table: normalise_category
Own<t_btree_ii__0_1__11__10> rel_2_normalise_category = mk<t_btree_ii__0_1__11__10>();
souffle::RelationWrapper<t_btree_ii__0_1__11__10> wrapper_rel_2_normalise_category;
// -- Table: user_package_category
Own<t_btree_i__0__1> rel_3_user_package_category = mk<t_btree_i__0__1>();
souffle::RelationWrapper<t_btree_i__0__1> wrapper_rel_3_user_package_category;
// -- Table: normalise_issue
Own<t_btree_i__0__1> rel_4_normalise_issue = mk<t_btree_i__0__1>();
souffle::RelationWrapper<t_btree_i__0__1> wrapper_rel_4_normalise_issue;
// -- Table: normalised_package_category
Own<t_btree_i__0__1> rel_5_normalised_package_category = mk<t_btree_i__0__1>();
souffle::RelationWrapper<t_btree_i__0__1> wrapper_rel_5_normalised_package_category;
public:
Sf_categorise()
: wrapper_rel_1_flora_category(0, *rel_1_flora_category, *this, "flora_category", std::array<const char *,3>{{"s:symbol","s:symbol","s:symbol"}}, std::array<const char *,3>{{"slug","name","synopsis"}}, 0)
, wrapper_rel_2_normalise_category(1, *rel_2_normalise_category, *this, "normalise_category", std::array<const char *,2>{{"s:symbol","s:symbol"}}, std::array<const char *,2>{{"user_input","normalised"}}, 0)
, wrapper_rel_3_user_package_category(2, *rel_3_user_package_category, *this, "user_package_category", std::array<const char *,1>{{"s:symbol"}}, std::array<const char *,1>{{"category"}}, 0)
, wrapper_rel_4_normalise_issue(3, *rel_4_normalise_issue, *this, "normalise_issue", std::array<const char *,1>{{"s:symbol"}}, std::array<const char *,1>{{"category"}}, 0)
, wrapper_rel_5_normalised_package_category(4, *rel_5_normalised_package_category, *this, "normalised_package_category", std::array<const char *,1>{{"s:symbol"}}, std::array<const char *,1>{{"category"}}, 0)
{
addRelation("flora_category", wrapper_rel_1_flora_category, false, true);
addRelation("normalise_category", wrapper_rel_2_normalise_category, false, false);
addRelation("user_package_category", wrapper_rel_3_user_package_category, true, false);
addRelation("normalise_issue", wrapper_rel_4_normalise_issue, false, true);
addRelation("normalised_package_category", wrapper_rel_5_normalised_package_category, false, true);
}
~Sf_categorise() {
}

private:
std::string             inputDirectory;
std::string             outputDirectory;
SignalHandler*          signalHandler {SignalHandler::instance()};
std::atomic<RamDomain>  ctr {};
std::atomic<std::size_t>     iter {};

void runFunction(std::string  inputDirectoryArg,
                 std::string  outputDirectoryArg,
                 bool         performIOArg,
                 bool         pruneImdtRelsArg) {
    this->inputDirectory  = std::move(inputDirectoryArg);
    this->outputDirectory = std::move(outputDirectoryArg);
    this->performIO       = performIOArg;
    this->pruneImdtRels   = pruneImdtRelsArg; 

    // set default threads (in embedded mode)
    // if this is not set, and omp is used, the default omp setting of number of cores is used.
#if defined(_OPENMP)
    if (0 < getNumThreads()) { omp_set_num_threads(getNumThreads()); }
#endif

    signalHandler->set();
// -- query evaluation --
{
 std::vector<RamDomain> args, ret;
subroutine_0(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_1(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_2(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_3(args, ret);
}
{
 std::vector<RamDomain> args, ret;
subroutine_4(args, ret);
}

// -- relation hint statistics --
signalHandler->reset();
}
public:
void run() override { runFunction("", "", false, false); }
public:
void runAll(std::string inputDirectoryArg = "", std::string outputDirectoryArg = "", bool performIOArg=true, bool pruneImdtRelsArg=true) override { runFunction(inputDirectoryArg, outputDirectoryArg, performIOArg, pruneImdtRelsArg);
}
public:
void printAll(std::string outputDirectoryArg = "") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","category"},{"auxArity","0"},{"name","normalise_issue"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"params\": [\"category\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"types\": [\"s:symbol\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_4_normalise_issue);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","category"},{"auxArity","0"},{"name","normalised_package_category"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"params\": [\"category\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"types\": [\"s:symbol\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_5_normalised_package_category);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","slug\tname\tsynopsis"},{"auxArity","0"},{"name","flora_category"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 3, \"params\": [\"slug\", \"name\", \"synopsis\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 3, \"types\": [\"s:symbol\", \"s:symbol\", \"s:symbol\"]}}"}});
if (!outputDirectoryArg.empty()) {directiveMap["output-dir"] = outputDirectoryArg;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_1_flora_category);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void loadAll(std::string inputDirectoryArg = "") override {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","category"},{"auxArity","0"},{"fact-dir","."},{"name","user_package_category"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"params\": [\"category\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"types\": [\"s:symbol\"]}}"}});
if (!inputDirectoryArg.empty()) {directiveMap["fact-dir"] = inputDirectoryArg;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_3_user_package_category);
} catch (std::exception& e) {std::cerr << "Error loading user_package_category data: " << e.what() << '\n';}
}
public:
void dumpInputs() override {
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "user_package_category";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_3_user_package_category);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
void dumpOutputs() override {
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "normalise_issue";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_4_normalise_issue);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "normalised_package_category";
rwOperation["types"] = "{\"relation\": {\"arity\": 1, \"auxArity\": 0, \"types\": [\"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_5_normalised_package_category);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
try {std::map<std::string, std::string> rwOperation;
rwOperation["IO"] = "stdout";
rwOperation["name"] = "flora_category";
rwOperation["types"] = "{\"relation\": {\"arity\": 3, \"auxArity\": 0, \"types\": [\"s:symbol\", \"s:symbol\", \"s:symbol\"]}}";
IOSystem::getInstance().getWriter(rwOperation, symTable, recordTable)->writeAll(*rel_1_flora_category);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
public:
SymbolTable& getSymbolTable() override {
return symTable;
}
RecordTable& getRecordTable() override {
return recordTable;
}
void setNumThreads(std::size_t numThreadsValue) override {
SouffleProgram::setNumThreads(numThreadsValue);
symTable.setNumLanes(getNumThreads());
recordTable.setNumLanes(getNumThreads());
}
void executeSubroutine(std::string name, const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) override {
if (name == "stratum_0") {
subroutine_0(args, ret);
return;}
if (name == "stratum_1") {
subroutine_1(args, ret);
return;}
if (name == "stratum_2") {
subroutine_2(args, ret);
return;}
if (name == "stratum_3") {
subroutine_3(args, ret);
return;}
if (name == "stratum_4") {
subroutine_4(args, ret);
return;}
fatal("unknown subroutine");
}
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_0(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
signalHandler->setMsg(R"_(flora_category("algorithms","Algorithms","Algorithms implemented in Haskell, like sorting, searching").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [23:1-23:106])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(0)),ramBitCast(RamSigned(1)),ramBitCast(RamSigned(2))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("bioinformatics","Bioinformatics","Tooling, algorithms and techniques for the domain of Bioinformatics").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [24:1-24:123])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(3)),ramBitCast(RamSigned(4)),ramBitCast(RamSigned(5))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("command-line","CLI & TUI tooling","Libraries to develope command-line interfaces").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [25:1-25:102])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(6)),ramBitCast(RamSigned(7)),ramBitCast(RamSigned(8))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("compilers-interpreters","Compilers and Interpreters","Tooling to create compilers and interpreters").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [26:1-26:120])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(9)),ramBitCast(RamSigned(10)),ramBitCast(RamSigned(11))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("cryptography","Cryptography","Algorithms for encrypting and hashing data").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [27:1-27:94])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(12)),ramBitCast(RamSigned(13)),ramBitCast(RamSigned(14))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("data-structures","Data Structures","Data structures, whether they are purely functional or mutable, there's always one for you").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [28:1-28:148])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(15)),ramBitCast(RamSigned(16)),ramBitCast(RamSigned(17))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("databases","Databases","Database drivers and interfaces").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [29:1-29:77])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(18)),ramBitCast(RamSigned(19)),ramBitCast(RamSigned(20))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("development","Development","Development helpers, integration with other languages").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [30:1-30:103])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(21)),ramBitCast(RamSigned(22)),ramBitCast(RamSigned(23))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("distributed","Distributed Systems & Computation","Tooling and techniques for writing distributed systems").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [31:1-31:126])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(24)),ramBitCast(RamSigned(25)),ramBitCast(RamSigned(26))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("ffi","FFI","Tooling to work with the Foreign Function Interface").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [32:1-32:85])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(27)),ramBitCast(RamSigned(28)),ramBitCast(RamSigned(29))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("game-dev","Game development","Libraries used for game development").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [33:1-33:87])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(30)),ramBitCast(RamSigned(31)),ramBitCast(RamSigned(32))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("generics","Generics","Tooling to work with Haskell's Generics").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [34:1-34:83])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(33)),ramBitCast(RamSigned(34)),ramBitCast(RamSigned(35))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("prelude","Prelude","Libraries that provide default imports").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [35:1-35:80])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(36)),ramBitCast(RamSigned(37)),ramBitCast(RamSigned(38))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("language","Language","Tooling for interfacing with other programming languages from Haskell programs").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [36:1-36:122])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(39)),ramBitCast(RamSigned(40)),ramBitCast(RamSigned(41))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("natural-language","Natural Language","Tooling to working with natural languages").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [37:1-37:101])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(42)),ramBitCast(RamSigned(43)),ramBitCast(RamSigned(44))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("network","Network Development","Connection pools, DNS, HTTP, API clients and network protocols").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [38:1-38:116])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(45)),ramBitCast(RamSigned(46)),ramBitCast(RamSigned(47))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("maths","Mathematics","Numerical and Mathematical packages").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [39:1-39:79])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(48)),ramBitCast(RamSigned(49)),ramBitCast(RamSigned(50))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("parser-implementations","Parser Implementations","Parsing data formats").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [40:1-40:92])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(51)),ramBitCast(RamSigned(52)),ramBitCast(RamSigned(53))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("parsers","Parsers","Libraries to ingest and parse data").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [41:1-41:75])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(54)),ramBitCast(RamSigned(55)),ramBitCast(RamSigned(56))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("parsing","Parsing","Parser generators, combinators and tools to help with parsing").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [42:1-42:103])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(57)),ramBitCast(RamSigned(58)),ramBitCast(RamSigned(59))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("system","System","Programming and communicating with the Operating System").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [43:1-43:95])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(60)),ramBitCast(RamSigned(61)),ramBitCast(RamSigned(62))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("testing","Testing","Test frameworks").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [44:1-44:57])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(63)),ramBitCast(RamSigned(64)),ramBitCast(RamSigned(65))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("text","Text","Working with textual data and algorithms").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [45:1-45:76])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(66)),ramBitCast(RamSigned(67)),ramBitCast(RamSigned(68))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("web","Web Development","Programming for the web").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [46:1-46:69])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(69)),ramBitCast(RamSigned(70)),ramBitCast(RamSigned(71))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();signalHandler->setMsg(R"_(flora_category("xml","XML","Libraries to consume and produce XML documents").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [47:1-47:80])_");
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
Tuple<RamDomain,3> tuple{{ramBitCast(RamSigned(72)),ramBitCast(RamSigned(73)),ramBitCast(RamSigned(74))}};
rel_1_flora_category->insert(tuple,READ_OP_CONTEXT(rel_1_flora_category_op_ctxt));
}
();if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","slug\tname\tsynopsis"},{"auxArity","0"},{"name","flora_category"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 3, \"params\": [\"slug\", \"name\", \"synopsis\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 3, \"types\": [\"s:symbol\", \"s:symbol\", \"s:symbol\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_1_flora_category);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_1(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
signalHandler->setMsg(R"_(normalise_category(X,X) :- 
   flora_category(_,X,_).
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [52:1-52:53])_");
if(!(rel_1_flora_category->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_1_flora_category_op_ctxt,rel_1_flora_category->createContext());
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
for(const auto& env0 : *rel_1_flora_category) {
Tuple<RamDomain,2> tuple{{ramBitCast(env0[1]),ramBitCast(env0[1])}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
}
();}
signalHandler->setMsg(R"_(normalise_category("Algorithm","Algorithms").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [53:1-53:47])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(75)),ramBitCast(RamSigned(1))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("Crypto","Cryptography").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [55:1-55:46])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(76)),ramBitCast(RamSigned(13))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("CLI","CLI & TUI Development").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [57:1-57:52])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(77)),ramBitCast(RamSigned(78))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("TUI","CLI & TUI Development").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [58:1-58:52])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(79)),ramBitCast(RamSigned(78))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("Command Line","CLI & TUI Development").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [60:1-60:61])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(80)),ramBitCast(RamSigned(78))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("CommandLine","CLI & TUI Development").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [61:1-61:60])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(81)),ramBitCast(RamSigned(78))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("Numeric","Mathematics").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [63:1-63:46])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(82)),ramBitCast(RamSigned(49))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("Numerical","Mathematics").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [64:1-64:48])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(83)),ramBitCast(RamSigned(49))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("Numerics","Mathematics").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [65:1-65:47])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(84)),ramBitCast(RamSigned(49))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("Arithmetic","Mathematics").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [66:1-66:49])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(85)),ramBitCast(RamSigned(49))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("Number Theory","Mathematics").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [67:1-67:52])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(86)),ramBitCast(RamSigned(49))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("Math","Mathematics").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [68:1-68:43])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(87)),ramBitCast(RamSigned(49))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("Mathematics","Mathematics").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [69:1-69:50])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(49)),ramBitCast(RamSigned(49))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("Maths","Mathematics").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [70:1-70:44])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(88)),ramBitCast(RamSigned(49))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("Parser Builder","Parsers").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [72:1-72:49])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(89)),ramBitCast(RamSigned(55))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("Parser Combinators","Parsers").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [73:1-73:53])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(90)),ramBitCast(RamSigned(55))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("Parser","Parsers").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [74:1-74:41])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(91)),ramBitCast(RamSigned(55))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("ParserCombinators","Parsers").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [75:1-75:52])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(92)),ramBitCast(RamSigned(55))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("Parsers","Parsers").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [76:1-76:42])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(55)),ramBitCast(RamSigned(55))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("Parsing","Parsers").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [77:1-77:42])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(58)),ramBitCast(RamSigned(55))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("Network","Network Development").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [79:1-79:54])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(93)),ramBitCast(RamSigned(46))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("Data Network","Network Development").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [80:1-80:59])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(94)),ramBitCast(RamSigned(46))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("Network APIs","Network Development").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [81:1-81:59])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(95)),ramBitCast(RamSigned(46))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("Network Control","Network Development").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [82:1-82:62])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(96)),ramBitCast(RamSigned(46))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("NetworkAPI","Network Development").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [83:1-83:57])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(97)),ramBitCast(RamSigned(46))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("NetworkAPIs","Network Development").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [84:1-84:58])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(98)),ramBitCast(RamSigned(46))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();signalHandler->setMsg(R"_(normalise_category("Networking","Network Development").
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [85:1-85:57])_");
[&](){
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
Tuple<RamDomain,2> tuple{{ramBitCast(RamSigned(99)),ramBitCast(RamSigned(46))}};
rel_2_normalise_category->insert(tuple,READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
}
();if (pruneImdtRels) rel_1_flora_category->purge();
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_2(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","category"},{"auxArity","0"},{"fact-dir","."},{"name","user_package_category"},{"operation","input"},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"params\": [\"category\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"types\": [\"s:symbol\"]}}"}});
if (!inputDirectory.empty()) {directiveMap["fact-dir"] = inputDirectory;}
IOSystem::getInstance().getReader(directiveMap, symTable, recordTable)->readAll(*rel_3_user_package_category);
} catch (std::exception& e) {std::cerr << "Error loading user_package_category data: " << e.what() << '\n';}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_3(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
signalHandler->setMsg(R"_(normalise_issue(category) :- 
   user_package_category(category),
   !normalise_category(category,_).
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [16:1-18:36])_");
if(!(rel_3_user_package_category->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_4_normalise_issue_op_ctxt,rel_4_normalise_issue->createContext());
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
CREATE_OP_CONTEXT(rel_3_user_package_category_op_ctxt,rel_3_user_package_category->createContext());
for(const auto& env0 : *rel_3_user_package_category) {
if( !(!rel_2_normalise_category->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt)).empty())) {
Tuple<RamDomain,1> tuple{{ramBitCast(env0[0])}};
rel_4_normalise_issue->insert(tuple,READ_OP_CONTEXT(rel_4_normalise_issue_op_ctxt));
}
}
}
();}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","category"},{"auxArity","0"},{"name","normalise_issue"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"params\": [\"category\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"types\": [\"s:symbol\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_4_normalise_issue);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
#ifdef _MSC_VER
#pragma warning(disable: 4100)
#endif // _MSC_VER
void subroutine_4(const std::vector<RamDomain>& args, std::vector<RamDomain>& ret) {
signalHandler->setMsg(R"_(normalised_package_category(normalised) :- 
   user_package_category(user_category),
   normalise_category(user_category,normalised).
in file /home/hecate/Projects/Flora/2-flora-server/cbits/categorise.dl [10:1-12:49])_");
if(!(rel_3_user_package_category->empty()) && !(rel_2_normalise_category->empty())) {
[&](){
CREATE_OP_CONTEXT(rel_5_normalised_package_category_op_ctxt,rel_5_normalised_package_category->createContext());
CREATE_OP_CONTEXT(rel_2_normalise_category_op_ctxt,rel_2_normalise_category->createContext());
CREATE_OP_CONTEXT(rel_3_user_package_category_op_ctxt,rel_3_user_package_category->createContext());
for(const auto& env0 : *rel_3_user_package_category) {
auto range = rel_2_normalise_category->lowerUpperRange_10(Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MIN_RAM_SIGNED)}},Tuple<RamDomain,2>{{ramBitCast(env0[0]), ramBitCast<RamDomain>(MAX_RAM_SIGNED)}},READ_OP_CONTEXT(rel_2_normalise_category_op_ctxt));
for(const auto& env1 : range) {
Tuple<RamDomain,1> tuple{{ramBitCast(env1[1])}};
rel_5_normalised_package_category->insert(tuple,READ_OP_CONTEXT(rel_5_normalised_package_category_op_ctxt));
}
}
}
();}
if (performIO) {
try {std::map<std::string, std::string> directiveMap({{"IO","file"},{"attributeNames","category"},{"auxArity","0"},{"name","normalised_package_category"},{"operation","output"},{"output-dir","."},{"params","{\"records\": {}, \"relation\": {\"arity\": 1, \"params\": [\"category\"]}}"},{"types","{\"ADTs\": {}, \"records\": {}, \"relation\": {\"arity\": 1, \"types\": [\"s:symbol\"]}}"}});
if (!outputDirectory.empty()) {directiveMap["output-dir"] = outputDirectory;}
IOSystem::getInstance().getWriter(directiveMap, symTable, recordTable)->writeAll(*rel_5_normalised_package_category);
} catch (std::exception& e) {std::cerr << e.what();exit(1);}
}
if (pruneImdtRels) rel_3_user_package_category->purge();
if (pruneImdtRels) rel_2_normalise_category->purge();
}
#ifdef _MSC_VER
#pragma warning(default: 4100)
#endif // _MSC_VER
};
SouffleProgram *newInstance_categorise(){return new Sf_categorise;}
SymbolTable *getST_categorise(SouffleProgram *p){return &reinterpret_cast<Sf_categorise*>(p)->getSymbolTable();}

#ifdef __EMBEDDED_SOUFFLE__
class factory_Sf_categorise: public souffle::ProgramFactory {
SouffleProgram *newInstance() {
return new Sf_categorise();
};
public:
factory_Sf_categorise() : ProgramFactory("categorise"){}
};
extern "C" {
factory_Sf_categorise __factory_Sf_categorise_instance;
}
}
#else
}
int main(int argc, char** argv)
{
try{
souffle::CmdOptions opt(R"(categorise.dl)",
R"()",
R"()",
false,
R"()",
1);
if (!opt.parse(argc,argv)) return 1;
souffle::Sf_categorise obj;
#if defined(_OPENMP) 
obj.setNumThreads(opt.getNumJobs());

#endif
obj.runAll(opt.getInputFileDir(), opt.getOutputFileDir());
return 0;
} catch(std::exception &e) { souffle::SignalHandler::instance()->error(e.what());}
}

#endif
