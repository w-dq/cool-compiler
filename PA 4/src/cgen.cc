
//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully. Make sure to
//    initialize the base class tags in
//       `CgenClassTable::CgenClassTable'
//
//    Add the label for the dispatch tables to
//       `IntEntry::code_def'
//       `StringEntry::code_def'
//       `BoolConst::code_def'
//
//    Add code to emit everyting else that is needed
//       in `CgenClassTable::code'
//
//
// The files as provided will produce code to begin the code
// segments, declare globals, and emit constants.  You must
// fill in the rest.
//
//**************************************************************

#include "cgen.h"
#include "cgen_gc.h"
using std::unordered_map;
using std::vector;

extern void emit_string_constant(ostream& str, char *s);
extern int cgen_debug;

//
// Three symbols from the semantic analyzer (semant.cc) are used.
// If e : No_type, then no code is generated for e.
// Special code is generated for new SELF_TYPE.
// The name "self" also generates code different from other references.
//
//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
Symbol 
       arg,
       arg2,
       Bool,
       concat,
       cool_abort,
       copy,
       Int,
       in_int,
       in_string,
       IO,
       length,
       Main,
       main_meth,
       No_class,
       No_type,
       Object,
       out_int,
       out_string,
       prim_slot,
       self,
       SELF_TYPE,
       Str,
       str_field,
       substr,
       type_name,
       val;
//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
  arg         = idtable.add_string("arg");
  arg2        = idtable.add_string("arg2");
  Bool        = idtable.add_string("Bool");
  concat      = idtable.add_string("concat");
  cool_abort  = idtable.add_string("abort");
  copy        = idtable.add_string("copy");
  Int         = idtable.add_string("Int");
  in_int      = idtable.add_string("in_int");
  in_string   = idtable.add_string("in_string");
  IO          = idtable.add_string("IO");
  length      = idtable.add_string("length");
  Main        = idtable.add_string("Main");
  main_meth   = idtable.add_string("main");
//   _no_class is a symbol that can't be the name of any 
//   user-defined class.
  No_class    = idtable.add_string("_no_class");
  No_type     = idtable.add_string("_no_type");
  Object      = idtable.add_string("Object");
  out_int     = idtable.add_string("out_int");
  out_string  = idtable.add_string("out_string");
  prim_slot   = idtable.add_string("_prim_slot");
  self        = idtable.add_string("self");
  SELF_TYPE   = idtable.add_string("SELF_TYPE");
  Str         = idtable.add_string("String");
  str_field   = idtable.add_string("_str_field");
  substr      = idtable.add_string("substr");
  type_name   = idtable.add_string("type_name");
  val         = idtable.add_string("_val");
}

static char *gc_init_names[] =
  { "_NoGC_Init", "_GenGC_Init", "_ScnGC_Init" };
static char *gc_collect_names[] =
  { "_NoGC_Collect", "_GenGC_Collect", "_ScnGC_Collect" };


//  BoolConst is a class that implements code generation for operations
//  on the two booleans, which are given global names here.
BoolConst falsebool(FALSE);
BoolConst truebool(TRUE);

//class for offset
struct Offset
{
  Symbol sym;
  int offset;
  Offset();
  Offset(Symbol, int);
};

//Offset construction
Offset::Offset()
{
  sym=No_type;
  offset=0;
}
Offset::Offset(Symbol c, int o)
{
  sym=c;
  offset=o;
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// attribute list
unordered_map<Symbol, vector<Symbol> >list_att;
// attribute table
unordered_map<Symbol, unordered_map<Symbol, int> > tab_att;
// array of classes 
// CgenClassTable(classes,os);
vector<CgenNodeP> nodes;
// ap from class name to node
unordered_map<Symbol, CgenNodeP>tab_CgenNode;
//dispatch list
unordered_map<Symbol, vector<Symbol> >list_disp;
//dispatch table
unordered_map<Symbol, unordered_map<Symbol, Offset> > tab_disp;
// scope for the let expression
vector<Symbol> scope_let;
//vector for parameter list
vector<Symbol> method_args;
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//traverse and push the method fromm the Features tre
void traverse_push(Features tre, Symbol sym, Symbol n)
{
  //traverse the feature tree
  for(int i = tre->first(); tre->more(i); i = tre->next(i))
  {
    method_class *method = dynamic_cast<method_class*>(tre->nth(i));
    if (method)
    {
      vector<Symbol> *tre = &(list_disp[sym]);
      unordered_map<Symbol, Offset> *map_node = &(tab_disp[sym]);
      auto iter = find((*tre).begin(), (*tre).end(), method->name);
      if (iter != (*tre).end())
      {
        //overiden
        (*map_node)[method->name].sym = n;      
      }
      else
      { 
        //push all the methods
        (*tre).push_back(method->name);
        (*map_node)[method->name] = Offset(n, (*map_node).size());
      }
    }
  }
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//reverse the symbols
int  reverse_sym(Symbol name)
{
  for (int i = scope_let.size() -1; i >=0; --i)
    if (scope_let[i] == name)
      return i;
  return -1;
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// push parent method and then self's method
void push_method(CgenNodeP node, Symbol sym)
{
  Symbol name = node->get_name();
  if (name != Object)
    push_method(node->get_parentnd(), sym);
  Features fs = node->features;
  traverse_push(fs,sym,name);
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//push the expressions out from let expression list
void push_out_let(Expressions actual)
{
  for (int i = 0; i < actual->len(); ++i)
  {
    scope_let.pop_back();
  }
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emmitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************

void program_class::cgen(ostream &os) 
{
  // spim wants comments to start with '#'
  os << "# start of generated code\n";

  initialize_constants();
  CgenClassTable *codegen_classtable = new CgenClassTable(classes,os);

  os << "\n# end of generated code\n";
}


//////////////////////////////////////////////////////////////////////////////
//
//  emit_* procedures
//
//  emit_X  writes code for operation "X" to the output stream.
//  There is an emit_X for each opcode X, as well as emit_ functions
//  for generating names according to the naming conventions (see emit.h)
//  and calls to support functions defined in the trap handler.
//
//  Register names and addresses are passed as strings.  See `emit.h'
//  for symbolic names you can use to refer to the strings.
//
//////////////////////////////////////////////////////////////////////////////

static void emit_load(char *dest_reg, int offset, char *source_reg, ostream& s)
{
  s << LW << dest_reg << " " << offset * WORD_SIZE << "(" << source_reg << ")" 
    << endl;
}

static void emit_store(char *source_reg, int offset, char *dest_reg, ostream& s)
{
  s << SW << source_reg << " " << offset * WORD_SIZE << "(" << dest_reg << ")"
      << endl;
}

static void emit_load_imm(char *dest_reg, int val, ostream& s)
{ s << LI << dest_reg << " " << val << endl; }

static void emit_load_address(char *dest_reg, char *address, ostream& s)
{ s << LA << dest_reg << " " << address << endl; }

static void emit_partial_load_address(char *dest_reg, ostream& s)
{ s << LA << dest_reg << " "; }

static void emit_load_bool(char *dest, const BoolConst& b, ostream& s)
{
  emit_partial_load_address(dest,s);
  b.code_ref(s);
  s << endl;
}

static void emit_load_string(char *dest, StringEntry *str, ostream& s)
{
  emit_partial_load_address(dest,s);
  str->code_ref(s);
  s << endl;
}

static void emit_load_int(char *dest, IntEntry *i, ostream& s)
{
  emit_partial_load_address(dest,s);
  i->code_ref(s);
  s << endl;
}

static void emit_move(char *dest_reg, char *source_reg, ostream& s)
{ s << MOVE << dest_reg << " " << source_reg << endl; }

static void emit_neg(char *dest, char *src1, ostream& s)
{ s << NEG << dest << " " << src1 << endl; }

static void emit_add(char *dest, char *src1, char *src2, ostream& s)
{ s << ADD << dest << " " << src1 << " " << src2 << endl; }

static void emit_addu(char *dest, char *src1, char *src2, ostream& s)
{ s << ADDU << dest << " " << src1 << " " << src2 << endl; }

static void emit_addiu(char *dest, char *src1, int imm, ostream& s)
{ s << ADDIU << dest << " " << src1 << " " << imm << endl; }

static void emit_div(char *dest, char *src1, char *src2, ostream& s)
{ s << DIV << dest << " " << src1 << " " << src2 << endl; }

static void emit_mul(char *dest, char *src1, char *src2, ostream& s)
{ s << MUL << dest << " " << src1 << " " << src2 << endl; }

static void emit_sub(char *dest, char *src1, char *src2, ostream& s)
{ s << SUB << dest << " " << src1 << " " << src2 << endl; }

static void emit_sll(char *dest, char *src1, int num, ostream& s)
{ s << SLL << dest << " " << src1 << " " << num << endl; }

static void emit_jalr(char *dest, ostream& s)
{ s << JALR << "\t" << dest << endl; }

static void emit_jal(char *address,ostream &s)
{ s << JAL << address << endl; }

static void emit_return(ostream& s)
{ s << RET << endl; }

static void emit_gc_assign(ostream& s)
{ s << JAL << "_GenGC_Assign" << endl; }

static void emit_disptable_ref(Symbol sym, ostream& s)
{  s << sym << DISPTAB_SUFFIX; }

static void emit_init_ref(Symbol sym, ostream& s)
{ s << sym << CLASSINIT_SUFFIX; }

static void emit_label_ref(int l, ostream &s)
{ s << "label" << l; }

static void emit_protobj_ref(Symbol sym, ostream& s)
{ s << sym << PROTOBJ_SUFFIX; }

static void emit_method_ref(Symbol classname, Symbol methodname, ostream& s)
{ s << classname << METHOD_SEP << methodname; }

static void emit_label_def(int l, ostream &s)
{
  emit_label_ref(l,s);
  s << ":" << endl;
}

static void emit_beqz(char *source, int label, ostream &s)
{
  s << BEQZ << source << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_beq(char *src1, char *src2, int label, ostream &s)
{
  s << BEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bne(char *src1, char *src2, int label, ostream &s)
{
  s << BNE << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bleq(char *src1, char *src2, int label, ostream &s)
{
  s << BLEQ << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blt(char *src1, char *src2, int label, ostream &s)
{
  s << BLT << src1 << " " << src2 << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_blti(char *src1, int imm, int label, ostream &s)
{
  s << BLT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_bgti(char *src1, int imm, int label, ostream &s)
{
  s << BGT << src1 << " " << imm << " ";
  emit_label_ref(label,s);
  s << endl;
}

static void emit_branch(int l, ostream& s)
{
  s << BRANCH;
  emit_label_ref(l,s);
  s << endl;
}

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//move stack pointer and store before function call
void move_sp_before(ostream& str)
{
    emit_addiu(SP, SP, -12, str);
    emit_store(FP, DEFAULT_OBJFIELDS, SP, str);
    emit_store(SELF, DISPTABLE_OFFSET, SP, str);
    emit_store(RA, 1, SP, str);
    emit_addiu(FP, SP, 4, str);
    emit_move(SELF, ACC, str);
}
//pop the stack after function call
void pop_sp_after(ostream& str)
{
    emit_load(FP, DEFAULT_OBJFIELDS, SP, str);
    emit_load(SELF, DISPTABLE_OFFSET, SP, str);
    emit_load(RA, STRING_SLOTS, SP, str);
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


//
// Push a register on the stack. The stack grows towards smaller addresses.
//
static void emit_push(char *reg, ostream& str)
{
  emit_store(reg,0,SP,str);
  emit_addiu(SP,SP,-4,str);
}

//
// Fetch the integer value in an Int object.
// Emits code to fetch the integer value of the Integer object pointed
// to by register source into the register dest
//
static void emit_fetch_int(char *dest, char *source, ostream& s)
{ emit_load(dest, DEFAULT_OBJFIELDS, source, s); }

//
// Emits code to store the integer value contained in register source
// into the Integer object pointed to by dest.
//
static void emit_store_int(char *source, char *dest, ostream& s)
{ emit_store(source, DEFAULT_OBJFIELDS, dest, s); }


static void emit_test_collector(ostream &s)
{
  emit_push(ACC, s);
  emit_move(ACC, SP, s); // stack end
  emit_move(A1, ZERO, s); // allocate nothing
  s << JAL << gc_collect_names[cgen_Memmgr] << endl;
  emit_addiu(SP,SP,4,s);
  emit_load(ACC,0,SP,s);
}

static void emit_gc_check(char *source, ostream &s)
{
  if (source != (char*)A1) emit_move(A1, source, s);
  s << JAL << "_gc_check" << endl;
}

///////////////////////////////////////////////////////////////////////////////
//
// coding strings, ints, and booleans
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type StringEntry.  StringEntry methods are defined both for String
// constant definitions and references.
//
// All integer constants are listed in the global "inttable" and have
// type IntEntry.  IntEntry methods are defined for Int
// constant definitions and references.
//
// Since there are only two Bool values, there is no need for a table.
// The two booleans are represented by instances of the class BoolConst,
// which defines the definition and reference methods for Bools.
//
///////////////////////////////////////////////////////////////////////////////

//
// Strings
//
void StringEntry::code_ref(ostream& s)
{
  s << STRCONST_PREFIX << index;
}

//
// Emit code for a constant String.
// You should fill in the code naming the dispatch table.
//

void StringEntry::code_def(ostream& s, int stringclasstag)
{
  IntEntryP lensym = inttable.add_int(len);

  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s  << LABEL                                             // label
      << WORD << stringclasstag << endl                                 // tag
      << WORD << (DEFAULT_OBJFIELDS + STRING_SLOTS + (len+4)/4) << endl // size
      << WORD;


 /***** Add dispatch information for class String ******/
      s << STRINGNAME << DISPTAB_SUFFIX << endl;                                            // dispatch table
      s << WORD;  lensym->code_ref(s);  s << endl;            // string length
  emit_string_constant(s,str);                                // ascii string
  s << ALIGN;                                                 // align to word
}

//
// StrTable::code_string
// Generate a string object definition for every string constant in the 
// stringtable.
//
void StrTable::code_string_table(ostream& s, int stringclasstag)
{  
  for (List<StringEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,stringclasstag);
}

//
// Ints
//
void IntEntry::code_ref(ostream &s)
{
  s << INTCONST_PREFIX << index;
}

//
// Emit code for a constant Integer.
// You should fill in the code naming the dispatch table.
//

void IntEntry::code_def(ostream &s, int intclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                // label
      << WORD << intclasstag << endl                      // class tag
      << WORD << (DEFAULT_OBJFIELDS + INT_SLOTS) << endl  // object size
      << WORD; 

 /***** Add dispatch information for class Int ******/
    s << INTNAME << DISPTAB_SUFFIX << endl;                                        // dispatch table
      s << WORD << str << endl;                           // integer value
}


//
// IntTable::code_string_table
// Generate an Int object definition for every Int constant in the
// inttable.
//
void IntTable::code_string_table(ostream &s, int intclasstag)
{
  for (List<IntEntry> *l = tbl; l; l = l->tl())
    l->hd()->code_def(s,intclasstag);
}


//
// Bools
//
BoolConst::BoolConst(int i) : val(i) { assert(i == 0 || i == 1); }

void BoolConst::code_ref(ostream& s) const
{
  s << BOOLCONST_PREFIX << val;
}
  
//
// Emit code for a constant Bool.
// You should fill in the code naming the dispatch table.
//

void BoolConst::code_def(ostream& s, int boolclasstag)
{
  // Add -1 eye catcher
  s << WORD << "-1" << endl;

  code_ref(s);  s << LABEL                                  // label
      << WORD << boolclasstag << endl                       // class tag
      << WORD << (DEFAULT_OBJFIELDS + BOOL_SLOTS) << endl   // object size
      << WORD;

 /***** Add dispatch information for class Bool ******/
    s << BOOLNAME << DISPTAB_SUFFIX << endl;                                          // dispatch table
      s << WORD << val << endl;                             // value (0 or 1)
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//***************************************************
//
//  Emit code to start the .data segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_data()
{
  Symbol main    = idtable.lookup_string(MAINNAME);
  Symbol string  = idtable.lookup_string(STRINGNAME);
  Symbol integer = idtable.lookup_string(INTNAME);
  Symbol boolc   = idtable.lookup_string(BOOLNAME);

  str << "\t.data\n" << ALIGN;
  //
  // The following global names must be defined first.
  //
  str << GLOBAL << CLASSNAMETAB << endl;
  str << GLOBAL; emit_protobj_ref(main,str);    str << endl;
  str << GLOBAL; emit_protobj_ref(integer,str); str << endl;
  str << GLOBAL; emit_protobj_ref(string,str);  str << endl;
  str << GLOBAL; falsebool.code_ref(str);  str << endl;
  str << GLOBAL; truebool.code_ref(str);   str << endl;
  str << GLOBAL << INTTAG << endl;
  str << GLOBAL << BOOLTAG << endl;
  str << GLOBAL << STRINGTAG << endl;

  //
  // We also need to know the tag of the Int, String, and Bool classes
  // during code generation.
  //
  str << INTTAG << LABEL
      << WORD << intclasstag << endl;
  str << BOOLTAG << LABEL 
      << WORD << boolclasstag << endl;
  str << STRINGTAG << LABEL 
      << WORD << stringclasstag << endl;    
}


//***************************************************
//
//  Emit code to start the .text segment and to
//  declare the global names.
//
//***************************************************

void CgenClassTable::code_global_text()
{
  str << GLOBAL << HEAP_START << endl
      << HEAP_START << LABEL 
      << WORD << 0 << endl
      << "\t.text" << endl
      << GLOBAL;
  emit_init_ref(idtable.add_string("Main"), str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Int"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("String"),str);
  str << endl << GLOBAL;
  emit_init_ref(idtable.add_string("Bool"),str);
  str << endl << GLOBAL;
  emit_method_ref(idtable.add_string("Main"), idtable.add_string("main"), str);
  str << endl;
}

void CgenClassTable::code_bools(int boolclasstag)
{
  falsebool.code_def(str,boolclasstag);
  truebool.code_def(str,boolclasstag);
}

void CgenClassTable::code_select_gc()
{
  //
  // Generate GC choice constants (pointers to GC functions)
  //
  str << GLOBAL << "_MemMgr_INITIALIZER" << endl;
  str << "_MemMgr_INITIALIZER:" << endl;
  str << WORD << gc_init_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_COLLECTOR" << endl;
  str << "_MemMgr_COLLECTOR:" << endl;
  str << WORD << gc_collect_names[cgen_Memmgr] << endl;
  str << GLOBAL << "_MemMgr_TEST" << endl;
  str << "_MemMgr_TEST:" << endl;
  str << WORD << (cgen_Memmgr_Test == GC_TEST) << endl;
}


//********************************************************
//
// Emit code to reserve space for and initialize all of
// the constants.  Class names should have been added to
// the string table (in the supplied code, is is done
// during the construction of the inheritance graph), and
// code for emitting string constants as a side effect adds
// the string's length to the integer table.  The constants
// are emmitted by running through the stringtable and inttable
// and producing code for each entry.
//
//********************************************************

void CgenClassTable::code_constants()
{
  //
  // Add constants that are required by the code generator.
  //
  stringtable.add_string("");
  inttable.add_string("0");

  stringtable.code_string_table(str,stringclasstag);
  inttable.code_string_table(str,intclasstag);
  code_bools(boolclasstag);
}


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//generate code of class
void CgenClassTable::code_class()
{
  str << CLASSNAMETAB << ":" << endl;
  //inverse the nds and store into nodes
  auto *l = nds;
  while(l!=NULL)
  {
    nodes.push_back(l->hd());
    l = l->tl();
  }
  //print the class names
  for (int i = nodes.size()-1; i >-1; --i)
  {
    auto name = nodes[i]->get_name();
    str << WORD; 
    stringtable.lookup_string(name->get_string())->code_ref(str); 
    str<< endl;
  }
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//generate code of object
void CgenClassTable::code_object()
{
  str << CLASSOBJTAB << LABEL;
  for (int i = nodes.size()-1; i >=0; --i)
  {
    str << WORD; emit_protobj_ref(nodes[i]->get_name(),str); str<<endl;
    str << WORD; emit_init_ref(nodes[i]->get_name(),str); str<<endl;
  }
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void CgenClassTable::code_dispatch()
{
  for (List<CgenNode> *l = nds; l; l = l->tl())
  {
    CgenNodeP node = l->hd();
    Symbol name=node->get_name();
    list_disp[name] = vector<Symbol>();
    tab_disp[name] = unordered_map<Symbol, Offset>();
    push_method(node,name);
  }
  for (List<CgenNode> *l = nds; l; l = l->tl())
  {
    CgenNodeP node = l->hd();
    Symbol name = node->get_name();
    emit_disptable_ref(name,str); str<< LABEL;
    vector<Symbol> tre = list_disp[name];
    for (auto iter:tre)
    { 
      Symbol sym = tab_disp[name][iter].sym;
      str << WORD; emit_method_ref(sym,iter,str); str<<endl; 
    }

  }
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
int get_num(CgenNodeP node)
{
  int num = 0;
  while(node->get_name() != No_class)
  {
    Features fs = node->features;
    for(int i = fs->first(); fs->more(i); i = fs->next(i)){
      attr_class *attr = dynamic_cast<attr_class*>(fs->nth(i));
      if (attr)
        ++num;
    }
    node = node->get_parentnd();
  }
  return num;
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void CgenClassTable::code_x_feature_proto(attr_class *attr){
      if (attr->type_decl == Str)
      {
        StringEntry *entry = stringtable.lookup_string("");
        str << WORD;
        entry->code_ref(str);
        str << endl;
        
      }
      else if (attr->type_decl == Bool)
      {
        str << WORD;
        falsebool.code_ref(str);
        str << endl;
        
      }
      else if (attr->type_decl == Int)
      {
        IntEntry *entry = inttable.lookup_string("0");
        str << WORD;
        entry->code_ref(str);
        str  << endl; 
        
      }
      else
      {
        str << WORD << 0 << endl;
      }
}

void CgenClassTable::code_node_proto(CgenNodeP node, Symbol sym)
{
  if (node->get_name() != Object)
  {
    code_node_proto(node->get_parentnd(), sym);
  }
  Features fs = node->features;
  for (int i = fs->first(); fs->more(i); i = fs->next(i))
  {
    attr_class *attr = dynamic_cast<attr_class*>(fs->nth(i));
    if (attr){
      code_x_feature_proto(attr);
      vector<Symbol> *attrs = &list_att[sym];
      int index = (*attrs).size();
      (*attrs).push_back(attr->name);
      unordered_map<Symbol, int> *map_offset = &tab_att[sym];
      (*map_offset)[attr->name] = index;
    }
  }
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void CgenClassTable::code_prototype()
{
  for (size_t i = 0; i < nodes.size(); i++) {
    str << WORD << "-1" << endl;

    int class_tag = nodes.size() - 1 - i;
    Symbol name = nodes[i]->get_name();
    int num_attr  = get_num(nodes[i]);

    
    emit_protobj_ref(name,str);str << LABEL;
    str << WORD << class_tag << endl;
    str << WORD << DEFAULT_OBJFIELDS + num_attr << endl;
    str << WORD; emit_disptable_ref(name,str); str<< endl;
    list_att[name]= vector<Symbol>();
    code_node_proto(nodes[i], name);
  }
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void CgenClassTable::code_x_feature_init(attr_class *attr){
  if (!(attr->init->get_type()))
  {
    if (attr->type_decl == Str )
    {
      emit_load_string(ACC, stringtable.lookup_string(""), str);
    }
    else if(attr->type_decl == Bool)
    {
      emit_load_bool(ACC, falsebool, str);
    }
    else if(attr->type_decl == Int)
    {
      emit_load_int(ACC, inttable.lookup_string("0"), str);
    }
    else
    {
      emit_load_imm(ACC, 0, str);
    }
  }
}


void CgenClassTable::code_init_class()
{
  for (List<CgenNode> *l = nds; l; l = l->tl()) {
    CgenNodeP node = l->hd();
    Symbol name = node->get_name();
    tab_CgenNode[name] = node;
    emit_init_ref(node->get_name(),str); str<<LABEL;


    
    move_sp_before(str);
    if (name != Object)
    {
      //when there is no parent
      char pa_label[128];
      Symbol name=node->get_parentnd()->get_name();
      sprintf(pa_label, "%s%s", name->get_string(), CLASSINIT_SUFFIX);
      emit_jal(pa_label, str);
    }

    Features fs = node->features;
    for(int i = fs->first(); fs->more(i); i = fs->next(i)) {
      attr_class *attr = dynamic_cast<attr_class*>(fs->nth(i));
      if (attr){
        attr->init->code(str);
        int offset = tab_att[name][attr->name] + DEFAULT_OBJFIELDS; 
        code_x_feature_init(attr);
        emit_store(ACC, offset, SELF, str);
      }
    }

    emit_move(ACC, SELF, str);
    pop_sp_after(str);
    emit_addiu(SP, SP, 12, str);
    emit_return(str); 
  }
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
void CgenClassTable::code_method()
{
  for (List<CgenNode> *l = nds; l; l = l->tl())
  {
    CgenNodeP node = l->hd();
    Symbol name = node->get_name();
    Features fs = node->features;

    if (name == Object || name == IO || name == Str)
      continue;
    current = name;
    for (int i = fs->first(); fs->more(i); i = fs->next(i))
    {
      method_class *method = dynamic_cast<method_class*>(fs->nth(i));
      if (method)
      {
        Formals fs = method->formals;
        method_args.clear();
        //store all parameters
        for (int i = fs->first(); fs->more(i); i = fs->next(i))
        {
          Symbol name=fs->nth(i)->get_name();
          method_args.push_back(name);
        }

        emit_method_ref(node->get_name(),method->name,str);str<<":"<<endl;
        move_sp_before(str);
        method->expr->code(str);
        pop_sp_after(str);
        emit_addiu(SP, SP, 12 + method->formals->len() * 4, str);
        emit_return(str);
      }
    }
      
  }
  
}
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

CgenClassTable::CgenClassTable(Classes classes, ostream& s) : nds(NULL) , str(s)
{
  // wudq: need change here
  // see the code produced by coolc
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   stringclasstag = 4;
   intclasstag =    2;
   boolclasstag =   3;
// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   enterscope();
   if (cgen_debug) cout << "Building CgenClassTable" << endl;
   install_basic_classes();
   install_classes(classes);
   build_inheritance_tree();

   code();  // wudq: all goes into code()
   exitscope();
}

void CgenClassTable::install_basic_classes()
{

// The tree package uses these globals to annotate the classes built below.
  //curr_lineno  = 0;
  Symbol filename = stringtable.add_string("<basic class>");

//
// A few special class names are installed in the lookup table but not
// the class list.  Thus, these classes exist, but are not part of the
// inheritance hierarchy.
// No_class serves as the parent of Object and the other special classes.
// SELF_TYPE is the self class; it cannot be redefined or inherited.
// prim_slot is a class known to the code generator.
//
  addid(No_class,
  new CgenNode(class_(No_class,No_class,nil_Features(),filename),
          Basic,this));
  addid(SELF_TYPE,
  new CgenNode(class_(SELF_TYPE,No_class,nil_Features(),filename),
          Basic,this));
  addid(prim_slot,
  new CgenNode(class_(prim_slot,No_class,nil_Features(),filename),
          Basic,this));

// 
// The Object class has no parent class. Its methods are
//        cool_abort() : Object    aborts the program
//        type_name() : Str        returns a string representation of class name
//        copy() : SELF_TYPE       returns a copy of the object
//
// There is no need for method bodies in the basic classes---these
// are already built in to the runtime system.
//
  install_class(
   new CgenNode(
    class_(Object, 
     No_class,
     append_Features(
           append_Features(
           single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
           single_Features(method(type_name, nil_Formals(), Str, no_expr()))),
           single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))),
     filename),
    Basic,this));

// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
   install_class(
    new CgenNode(
     class_(IO, 
            Object,
            append_Features(
            append_Features(
            append_Features(
            single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())),
            single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))),
            single_Features(method(in_string, nil_Formals(), Str, no_expr()))),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
     filename),     
    Basic,this));

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
   install_class(
    new CgenNode(
     class_(Int, 
      Object,
            single_Features(attr(val, prim_slot, no_expr())),
      filename),
     Basic,this));

//
// Bool also has only the "val" slot.
//
    install_class(
     new CgenNode(
      class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())),filename),
      Basic,this));

//
// The class Str has a number of slots and operations:
//       val                                  ???
//       str_field                            the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
   install_class(
    new CgenNode(
      class_(Str, 
       Object,
             append_Features(
             append_Features(
             append_Features(
             append_Features(
             single_Features(attr(val, Int, no_expr())),
            single_Features(attr(str_field, prim_slot, no_expr()))),
            single_Features(method(length, nil_Formals(), Int, no_expr()))),
            single_Features(method(concat, 
           single_Formals(formal(arg, Str)),
           Str, 
           no_expr()))),
      single_Features(method(substr, 
           append_Formals(single_Formals(formal(arg, Int)), 
              single_Formals(formal(arg2, Int))),
           Str, 
           no_expr()))),
       filename),
        Basic,this));

}

// CgenClassTable::install_class
// CgenClassTable::install_classes
//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_class(CgenNodeP nd)
{
  Symbol name = nd->get_name();

  if (probe(name))
    {
      return;
    }

  // The class name is legal, so add it to the list of classes
  // and the symbol table.
  nds = new List<CgenNode>(nd,nds);
  addid(name,nd);
}

void CgenClassTable::install_classes(Classes cs)
{
  for(int i = cs->first(); cs->more(i); i = cs->next(i))
    install_class(new CgenNode(cs->nth(i),NotBasic,this));
}

//
// CgenClassTable::build_inheritance_tree
//
void CgenClassTable::build_inheritance_tree()
{
  for(List<CgenNode> *l = nds; l; l = l->tl())
      set_relations(l->hd());
}

//
// CgenClassTable::set_relations
//
// Takes a CgenNode and locates its, and its parent's, inheritance nodes
// via the class table.  Parent and child pointers are added as appropriate.
//
void CgenClassTable::set_relations(CgenNodeP nd)
{
  CgenNode *parent_node = probe(nd->get_parent());
  nd->set_parentnd(parent_node);
  parent_node->add_child(nd);
}

void CgenNode::add_child(CgenNodeP n)
{
  children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNodeP p)
{
  assert(parentnd == NULL);
  assert(p != NULL);
  parentnd = p;
}



void CgenClassTable::code()
{
  for (int i=0; i<NUM_GEN; ++i)
  {
    if (cgen_debug) cout << code_name[i] << endl;
  	switch (i)
    {
      case 0:
      code_global_data();
      break;
      case 1:
      code_select_gc();
      break;
      case 2:
      code_constants();
      break;
      case 3:
      code_class();
      break;
      case 4:
      code_object();
      break;
      case 5:
      code_dispatch();
      break;
      case 6:
      code_prototype();
      break;
      case 7:
      code_global_text();
      break;
      case 8:
      code_init_class();
      break;
      case 9:
      code_method();
      break;
    }
  }
}


CgenNodeP CgenClassTable::root()
{
   return probe(Object);
}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTableP ct) :
   class__class((const class__class &) *nd),
   parentnd(NULL),
   children(NULL),
   basic_status(bstatus)
{ 
   stringtable.add_string(name->get_string());          // Add class name to string table
}


//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.h'  Sample code for
//   constant integers, strings, and booleans are provided.
//
//*****************************************************************
//generate code about four operations
void four_opertion(ostream &s, Expression e1, Expression e2, OPERATOR e)
{
  e1->code(s);
  emit_push(ACC, s); 
  scope_let.push_back(No_type);

  e2->code(s);
  emit_jal("Object.copy", s); 
  emit_load(T1, 1, SP, s); 
  emit_addiu(SP, SP, 4, s);
  scope_let.pop_back();

  emit_load(T1, DEFAULT_OBJFIELDS, T1, s);
  emit_load(T2, DEFAULT_OBJFIELDS, ACC, s); 
  switch (e)
  {
  case ADD_O:
    emit_addu(T1, T1, T2, s);
    break;
  case SUB_O:
    emit_sub(T1, T1, T2, s); 
    break;
  case MUL_O:
    emit_mul(T1, T1, T2, s);  
    break;
  case DIV_O:
    emit_div(T1, T1, T2, s);
    break;
  }
  emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
}

void assign_class::code(ostream &s)
{
  expr->code(s);
	int offset;

	int id_global = reverse_sym(name);
	if (id_global != -1)
  {
		offset = scope_let.size() - id_global ;
		emit_store(ACC, offset, SP, s);
		return;
	}
	vector<Symbol>::iterator iter;
	if((iter = find(method_args.begin(), method_args.end(), name))
			!= method_args.end()) {
		offset = method_args.end() - iter + 2;
		emit_load(ACC, offset, FP, s);
		return;
	}
	
  offset = tab_att[current][name] + 3;
  
	emit_store(ACC, offset, SELF, s);
}

void static_dispatch_class::code(ostream &s)
{
  for (int i = actual->first(); actual->more(i); i = actual->next(i))
  {
    actual->nth(i)->code(s);
    emit_push(ACC, s);
    scope_let.push_back(No_type);
  }
  
  expr->code(s);

  emit_bne(ACC, ZERO, id_global, s);
  //sprint the filename
  char buf[2000];
  sprintf(buf, "%s0", STRCONST_PREFIX);
  emit_load_address(ACC, buf, s); 
  emit_load_imm(T1, 1, s);
  emit_jal("_dispatch_abort", s);
  emit_label_def(id_global, s);
  ++id_global;

  Symbol type = expr->get_type();
  if (type == SELF_TYPE)
    type = current;
  
  int offset  = tab_disp[type_name][name].offset;
  sprintf(buf, "%s%s", type_name->get_string(), DISPTAB_SUFFIX);
  emit_load_address(T1, buf, s);
  emit_load(T1, offset, T1, s);
  emit_jalr(T1, s);
  push_out_let(actual);

}

void dispatch_class::code(ostream &s)
{
  //traverse the AST and push all the parameters
  for (int i = actual->first(); actual->more(i); i = actual->next(i))
  {
    actual->nth(i)->code(s);
    emit_push(ACC, s);
    scope_let.push_back(No_type);
  }
  

  expr->code(s);
  char buf[2000];

  emit_bne(ACC, ZERO, id_global, s);
  //sprint the filename
  sprintf(buf, "%s0", STRCONST_PREFIX);
  emit_load_address(ACC, buf, s); 
  emit_load_imm(T1, 1, s);
  emit_jal("_dispatch_abort", s);
  emit_label_def(id_global, s);
  emit_load(T1, 2, ACC, s);
  ++id_global;

  Symbol type = expr->get_type();
  if (type == SELF_TYPE) 
    type = current;

  int offset  = tab_disp[type][name].offset;
  emit_load(T1, offset, T1, s);
  emit_jalr(T1, s);
  push_out_let(actual);
}


void cond_class::code(ostream &s)
{
  pred->code(s);
  int then_ = id_global;
  ++id_global;
  int else_ = id_global;
  ++id_global;
  emit_load(T1, 3, ACC, s);
  emit_beqz(T1, then_, s);

  then_exp->code(s); emit_branch(else_, s); emit_label_def(then_, s);
  else_exp->code(s); emit_label_def(else_, s);
}

void loop_class::code(ostream &s)
{
  int while_index = id_global;
  ++id_global;
  int end_index = id_global;
  ++id_global;
  emit_label_def(while_index, s);
  pred->code(s); emit_load(T1, 3, ACC, s); emit_load_imm(ACC, 0, s); emit_beqz(T1, end_index, s);
  body->code(s); emit_branch(while_index, s);  emit_label_def(end_index, s);
}

void typcase_class::code(ostream &s)
{

  expr->code(s);

  int case_label = id_global;
  ++id_global;
  emit_bne(ACC, ZERO, case_label, s); emit_load_address(ACC, "str_const0", s); emit_load_imm(T1, 1, s);
  emit_jal("_case_abort2", s); emit_label_def(case_label, s);
  
  emit_push(ACC, s);

  int end_label = id_global;
  ++id_global;
  for(int i = cases->first(); cases->more(i); i = cases->next(i))
  {
    branch_class* branch = static_cast<branch_class*>(cases->nth(i));
    
    int next_case = id_global;
    ++id_global;
    
  
    emit_load(T1, 1, SP, s); 
    emit_load(T1, 0, T1, s);
    char buf[2000];

    sprintf(buf, "%s%s", branch->type_decl->get_string(), PROTOBJ_SUFFIX);
    emit_load_address(T2, buf, s); 
    emit_load(T2, 0, T2, s);

    emit_bne(T1 , T2, next_case, s);

    scope_let.push_back(branch->name);  
    branch->expr->code(s);

    emit_branch(end_label, s); 
    emit_label_def(next_case, s);
    scope_let.pop_back();
  }


  emit_jal("_case_abort", s);
  emit_load_imm(ACC, 0, s); 
  emit_label_def(end_label, s);
  emit_addiu(SP, SP, 4, s);
  return;
}

void block_class::code(ostream &s)
{
  for (int i = body->first(); body->more(i); i = body->next(i))
  {
    body->nth(i)->code(s);
  }
}

void let_class::code(ostream &s)
{
  if(init->get_type())
  {
    init->code(s);
  }
  else
  {
    if(type_decl == Bool)
    {
      emit_load_bool(ACC, falsebool, s);
    }
    else if(type_decl == Int)
    {
      emit_load_int(ACC, inttable.lookup_string("0"), s);
    }
    else if (type_decl == Str)
    {
      emit_load_string(ACC, stringtable.lookup_string(""), s);
    }
    else
    {
      emit_load_imm(ACC, 0, s);
    }
  }

  emit_push(ACC, s);
  scope_let.push_back(identifier);
  body->code(s);
  scope_let.pop_back();
  emit_addiu(SP, SP, 4, s);
}

void plus_class::code(ostream &s)
{
  four_opertion(s,e1,e2,ADD_O);
}

void sub_class::code(ostream &s)
{
  four_opertion(s,e1,e2,SUB_O);
}

void mul_class::code(ostream &s)
{
  four_opertion(s,e1,e2,MUL_O);
}

void divide_class::code(ostream &s)
{
  four_opertion(s,e1,e2,DIV_O);
}

void neg_class::code(ostream &s) 
{
  e1->code(s);
  emit_jal("Object.copy", s); 
  emit_load(T1, DEFAULT_OBJFIELDS, ACC, s);
  emit_neg(T1, T1, s);
  emit_store(T1, DEFAULT_OBJFIELDS, ACC, s);
}


void lt_class::code(ostream &s)
{
  e1->code(s);
  emit_push(ACC, s);
  scope_let.push_back(No_type);
  e2->code(s);

  emit_move(T2, ACC, s); 
  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);

  scope_let.pop_back();
  
  int index = id_global++;
  emit_load(T1, DEFAULT_OBJFIELDS, T1, s);
  emit_load(T2, DEFAULT_OBJFIELDS, T2, s);
  emit_load_address(ACC,"bool_const1", s); 
  emit_blt(T1, T2, index, s);
  emit_load_address(ACC, "bool_const0", s);
  emit_label_def(index, s);
}


void eq_class::code(ostream &s)
{
  e1->code(s);
  emit_push(ACC, s);
  scope_let.push_back(No_type);
  e2->code(s);
  emit_move(T2, ACC, s);
  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);
  scope_let.pop_back();
  Symbol t = e1->get_type();
  if (t == Int || t == Bool || t == Str)
  {
    emit_load_address(ACC,"bool_const1", s); 
    emit_load_address(A1, "bool_const0", s);
    emit_jal("equality_test",s);
  }
  else
  {  
    int index = id_global++;
    emit_load_address(ACC, "bool_const1",s); 
    emit_beq(T1, T2, index, s); 
    emit_load_address(ACC, "bool_const0", s);
    emit_label_def(index, s);
  }

}


void leq_class::code(ostream &s)
{
  e1->code(s);
  emit_push(ACC, s); 
  scope_let.push_back(No_type);
  e2->code(s);
  emit_move(T2, ACC, s); 
  emit_load(T1, 1, SP, s);
  emit_addiu(SP, SP, 4, s);
  scope_let.pop_back();
  
  int index = id_global++;
  emit_load(T1, 3, T1, s);
  emit_load(T2, 3, T2, s);
  emit_load_address(ACC,"bool_const1", s); 
  emit_bleq(T1, T2, index, s);
  emit_load_address(ACC, "bool_const0", s);
  emit_label_def(index, s);
}

void comp_class::code(ostream &s)
{
  int index = id_global++;

  e1->code(s);
  emit_jal("Object.copy", s); 
  emit_load(T1, 3, ACC, s);
  emit_beqz(T1, index, s);
  emit_load_address(ACC, "bool_const0", s);
  emit_label_def(index, s);
  emit_load_address(ACC, "bool_const1", s); 
}

void int_const_class::code(ostream& s)
{
  char* str1=token->get_string();
  IntEntry* sym=inttable.lookup_string(str1);
  emit_load_int(ACC,sym , s); 
}


void string_const_class::code(ostream& s)
{
  char* str1=token->get_string();
  StringEntry* sym=stringtable.lookup_string(str1);
  emit_load_string(ACC, sym, s);
}

void bool_const_class::code(ostream& s)
{
  emit_load_bool(ACC, BoolConst(val), s);
}

void new__class::code(ostream &s)
{
  if (type_name == SELF_TYPE)
  {
    emit_move(T1, SELF, s);
    emit_load(T1, 0, T1, s);
    emit_sll(T1, T1, DEFAULT_OBJFIELDS, s);
    emit_load_address(T2, "class_objTab", s);
    emit_addu(T1, T1, T2, s);
    emit_load(ACC, 0, T1, s);
    emit_load(T2, 1, T1, s); 
    emit_push(T2, s); 
    emit_jal("Object.copy", s);
    emit_load(T2, 1, SP, s);
    emit_addiu(SP, SP, 4, s);
    emit_jalr(T2, s);
  }
  else
  {
    char object[64];
    char object_init[64];
    char *name = type_name->get_string();
    sprintf(object, "%s%s", name, PROTOBJ_SUFFIX);
    sprintf(object_init, "%s%s", name, CLASSINIT_SUFFIX);
    emit_load_address(ACC, object, s);  
    emit_jal("Object.copy", s); 
    emit_jal(object_init, s); 
  }
}

void isvoid_class::code(ostream &s)
{
  int index = id_global++;
  e1->code(s);
  emit_move(T1, ACC, s);
  char buf[2000];
  sprintf(buf, "%s%d", BOOLCONST_PREFIX, TRUE);
  emit_load_address(ACC, buf, s);
  emit_beqz(T1, index, s);
  sprintf(buf, "%s%d", BOOLCONST_PREFIX, FALSE);
  emit_load_address(ACC, buf, s);
  emit_label_def(index, s);
}


void no_expr_class::code(ostream &s)
{
  return;
}

void object_class::code(ostream &s)
{
  if (name == self)
  {
		emit_move(ACC, SELF, s);
		return;
	}

	int offset;
	vector<Symbol>::iterator iter;
	int id_global = reverse_sym(name);
	if (id_global != -1)
  {
	
		offset = scope_let.size() - id_global ;
		emit_load(ACC, offset, SP, s);
		return;
	}

	if((iter = find(method_args.begin(), method_args.end(), name))
			!= method_args.end()) {
		offset = method_args.end() - iter  + 2;
		emit_load(ACC, offset, FP, s);  
		return;
	}

	offset = tab_att[current][name] + DEFAULT_OBJFIELDS;
	emit_load(ACC, offset, SELF, s);
}