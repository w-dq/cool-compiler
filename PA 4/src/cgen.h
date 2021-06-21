#include <assert.h>
#include <stdio.h>
#include "emit.h"
#include "cool-tree.h"
#include "symtab.h"

#include <assert.h>
#include <algorithm>
#include <unordered_map>
#include <list>
#include <stdio.h>
#include <sstream>
#include <string>
#include <stack>
#include <vector>
using std::string;

enum Basicness     {Basic, NotBasic};
#define TRUE 1
#define FALSE 0
#define NUM_GEN 10
int id_global = 0;

class CgenClassTable;
typedef CgenClassTable *CgenClassTableP;

class CgenNode;
typedef CgenNode *CgenNodeP;

Symbol current;
class CgenClassTable : public SymbolTable<Symbol,CgenNode> {
private:
   List<CgenNode> *nds;
   ostream& str;
   int stringclasstag;
   int intclasstag;
   int boolclasstag;


// The following methods emit code for
// constants and global declarations.

   
   void code_bools(int);
   void code_global_data();
   void code_select_gc();
   void code_constants();
   void code_class();
   void code_object();
   void code_dispatch();
   void code_prototype();
   void code_global_text();
   void code_init_class();
   void code_method();
   void code_node_proto(CgenNodeP node, Symbol sym);
   void code_x_feature_proto(attr_class *attr);
   void code_x_feature_init(attr_class *attr);
//////

// The following creates an inheritance graph from
// a list of classes.  The graph is implemented as
// a tree of `CgenNode', and class names are placed
// in the base class symbol table.

   void install_basic_classes();
   void install_class(CgenNodeP nd);
   void install_classes(Classes cs);
   void build_inheritance_tree();
   void set_relations(CgenNodeP nd);
public:
   CgenClassTable(Classes, ostream& str);
   void code();
   CgenNodeP root();
};

string code_name[NUM_GEN]={
      "coding global data",
      "choosing gc",
      "coding constants",
      "coding class_nameTab",
      "coding clas_objTab",
      "coding class_dispatchTab",
      "coding prototype objects",
      "coding global text",
      "coding class initializer",
      "coding class method"
};

enum OPERATOR
{
  ADD_O,SUB_O,MUL_O,DIV_O
};

class CgenNode : public class__class {
private: 
   CgenNodeP parentnd;                        // Parent of class
   List<CgenNode> *children;                  // Children of class
   Basicness basic_status;                    // `Basic' if class is basic
                                              // `NotBasic' otherwise

public:
   CgenNode(Class_ c,
            Basicness bstatus,
            CgenClassTableP class_table);

   void add_child(CgenNodeP child);
   List<CgenNode> *get_children() { return children; }
   void set_parentnd(CgenNodeP p);
   CgenNodeP get_parentnd() { return parentnd; }
   int basic() { return (basic_status == Basic); }
};

class BoolConst 
{
 private: 
  int val;
 public:
  BoolConst(int);
  void code_def(ostream&, int boolclasstag);
  void code_ref(ostream&) const;
};


void traverse_push(Features, Symbol, Symbol);
void push_method(CgenNodeP, Symbol);
int get_num(CgenNodeP);
int reverse_sym(Symbol);
int get_num(CgenNodeP);
