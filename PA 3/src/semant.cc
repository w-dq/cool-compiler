


#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <vector>
#include <list>
#include <set>
#include "semant.h"
#include "utilities.h"

extern int semant_debug;
extern char *curr_filename;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.
//
//////////////////////////////////////////////////////////////////////
static Symbol 
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
    val
;

static Class_ curr_class = NULL;
// static ClassTable* classtable;

typedef std::map<Symbol, SymbolTable<Symbol, method_class>> MethodTables;
typedef SymbolTable<Symbol, Symbol> Attribtable;

static Attribtable attribtable;
static MethodTables methodtables;

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


// "Classes classes" is a list of classes.
ClassTable::ClassTable(Classes classes) : semant_errors(0) , error_stream(cerr) {
    
    install_basic_classes();

    // Insert all the classes into m_classes.
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {

        // class name cannot be SELF_TYPE
        if (classes->nth(i)->GetName() == SELF_TYPE || classes->nth(i)->GetName() == Object || classes->nth(i)->GetName() == IO ||
            classes->nth(i)->GetName() == Int || classes->nth(i)->GetName() == Bool || classes->nth(i)->GetName() == Str) {
            semant_error(classes->nth(i)) << "Redefinition of basic class "<<classes->nth(i)->GetName()<<".\n";
            return;
        }

        // class cannot be declared before
        if (m_classes.find(classes->nth(i)->GetName()) == m_classes.end()) {
            m_classes.insert(std::make_pair(classes->nth(i)->GetName(), classes->nth(i)));
        } else {
            semant_error(classes->nth(i)) << "Class " << classes->nth(i)->GetName() << " was previously defined.\n";
            return;
        }

    }

    // Check the inheritance one by one.
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        curr_class = classes->nth(i);
        Symbol parent = curr_class->GetParent();
        while (parent != Object && parent != classes->nth(i)->GetName()) {
            if (parent == Int || parent == Str || parent == SELF_TYPE || parent == Bool) {
                semant_error(curr_class) << "Class " << curr_class->GetName() << " cannot inherit class " << parent << ".\n";
                return;
            }

            // check that the parent of curr_class is present in m_classes
            if (m_classes.find(parent) == m_classes.end()) {
                semant_error(curr_class) << "Class " << curr_class->GetName() << " inherits from an undefined class " << parent << ".\n";
                return;
            }

            curr_class = m_classes[parent];
            parent = curr_class->GetParent();

        }

        if (parent != Object){
            semant_error(curr_class) << "Class Hash, or an ancestor of "  << curr_class->GetName() <<", is involved in an inheritance cycle.";
            return;
        }

    }
}

bool ClassTable::CheckInheritance(Symbol ancestor, Symbol child) {
    if (ancestor == SELF_TYPE) {
        return child == SELF_TYPE;
    }

    if (child == SELF_TYPE) {
        child = curr_class->GetName();
    }

    for (; child != No_class; child = m_classes.find(child)->second->GetParent()) {
        if (child == ancestor) {
            return true;
        }
    }
    return false;
}

std::list<Symbol> ClassTable::GetInheritancePath(Symbol type) {
    if (type == SELF_TYPE) {
        type = curr_class->GetName();
    }

    std::list<Symbol> path;

    for (; type != No_class; type = m_classes[type]->GetParent()) {
        path.push_front(type);  
    }

    return path;
}

Symbol ClassTable::FindCommonAncestor(Symbol type1, Symbol type2) {

    std::list<Symbol> path1 = GetInheritancePath(type1);
    std::list<Symbol> path2 = GetInheritancePath(type2);

    Symbol ret;
    std::list<Symbol>::iterator iter1 = path1.begin(),
                                iter2 = path2.begin();

    while (iter1 != path1.end() && iter2 != path2.end()) {
        if (*iter1 == *iter2) {
            ret = *iter1;
        } else {
            break;
        }

        iter1++;
        iter2++;
    }

    return ret;
}


void ClassTable::install_basic_classes() {

    // The tree package uses these globals to annotate the classes built below.
   // curr_lineno  = 0;
    Symbol filename = stringtable.add_string("<basic class>");

    // The following demonstrates how to create dummy parse trees to
    // refer to basic Cool classes.  There's no need for method
    // bodies -- these are already built into the runtime system.

    // IMPORTANT: The results of the following expressions are
    // stored in local variables.  You will want to do something
    // with those variables at the end of this method to make this
    // code meaningful.

    // 
    // The Object class has no parent class. Its methods are
    //        abort() : Object    aborts the program
    //        type_name() : Str   returns a string representation of class name
    //        copy() : SELF_TYPE  returns a copy of the object
    //
    // There is no need for method bodies in the basic classes---these
    // are already built in to the runtime system.

    Class_ Object_class =
    class_(
        Object, 
        No_class,
        append_Features(
            append_Features(
                single_Features(method(cool_abort, nil_Formals(), Object, no_expr())),
                single_Features(method(type_name, nil_Formals(), Str, no_expr()))
            ),
            single_Features(method(copy, nil_Formals(), SELF_TYPE, no_expr()))
        ),
        filename
    );

    // 
    // The IO class inherits from Object. Its methods are
    //        out_string(Str) : SELF_TYPE       writes a string to the output
    //        out_int(Int) : SELF_TYPE            "    an int    "  "     "
    //        in_string() : Str                 reads a string from the input
    //        in_int() : Int                      "   an int     "  "     "
    //
    Class_ IO_class = 
    class_(
        IO, 
        Object,
        append_Features(
            append_Features(
                append_Features(
                    single_Features(method(out_string, single_Formals(formal(arg, Str)),
                        SELF_TYPE, no_expr())
                ),
                    single_Features(method(out_int, single_Formals(formal(arg, Int)),
                        SELF_TYPE, no_expr()))
                ),
                single_Features(method(in_string, nil_Formals(), Str, no_expr()))
            ),
            single_Features(method(in_int, nil_Formals(), Int, no_expr()))
        ),
        filename
    );  

    //
    // The Int class has no methods and only a single attribute, the
    // "val" for the integer. 
    //
    Class_ Int_class =
    class_(
        Int, 
        Object,
        single_Features(attr(val, prim_slot, no_expr())),
        filename
    );

    //
    // Bool also has only the "val" slot.
    //
    Class_ Bool_class =
    class_(Bool, Object, single_Features(attr(val, prim_slot, no_expr())), filename);

    //
    // The class Str has a number of slots and operations:
    //       val                                  the length of the string
    //       str_field                            the string itself
    //       length() : Int                       returns length of the string
    //       concat(arg: Str) : Str               performs string concatenation
    //       substr(arg: Int, arg2: Int): Str     substring selection
    //       
    Class_ Str_class =
    class_(
        Str, 
        Object,
        append_Features(
            append_Features(
                append_Features(
                    append_Features(
                        single_Features(attr(val, Int, no_expr())),
                        single_Features(attr(str_field, prim_slot, no_expr()))
                        ),
                    single_Features(method(length, nil_Formals(), Int, no_expr()))
                    ),
                single_Features(method(
                    concat, 
                    single_Formals(formal(arg, Str)),
                    Str, 
                    no_expr()
                    ))
                ),
            single_Features(method(
                substr, 
                append_Formals(
                    single_Formals(formal(arg, Int)), 
                    single_Formals(formal(arg2, Int))
                ),
                Str, 
                no_expr()
            ))
        ),
        filename
    );

    m_classes.insert(std::make_pair(Object, Object_class));
    m_classes.insert(std::make_pair(IO, IO_class));
    m_classes.insert(std::make_pair(Int, Int_class));
    m_classes.insert(std::make_pair(Bool, Bool_class));
    m_classes.insert(std::make_pair(Str, Str_class));

}

////////////////////////////////////////////////////////////////////
//
// semant_error is an overloaded function for reporting errors
// during semantic analysis.  There are three versions:
//
//    ostream& ClassTable::semant_error()                
//
//    ostream& ClassTable::semant_error(Class_ c)
//       print line number and filename for `c'
//
//    ostream& ClassTable::semant_error(Symbol filename, tree_node *t)  
//       print a line number and filename
//
///////////////////////////////////////////////////////////////////

ostream& ClassTable::semant_error(Class_ c)
{                                                       
    return semant_error(c->get_filename(),c);
}

ostream& ClassTable::semant_error(Symbol filename, tree_node *t)
{
    error_stream << filename << ":" << t->get_line_number() << ": ";
    return semant_error();
}

ostream& ClassTable::semant_error()                  
{                                                 
    semant_errors++;                            
    return error_stream;
}

void method_class::AddMethodToTable(Symbol class_name, ClassTable *classtable) {
    methodtables[class_name].addid(name, new method_class(copy_Symbol(name), formals->copy_list(), copy_Symbol(return_type), expr->copy_Expression()));
}

void attr_class::AddAttribToTable(Symbol class_name, ClassTable *classtable) {
    if (name == self) {
        classtable->semant_error(curr_class) << "'self' cannot be the name of an attribute.\n";
    }
    if (attribtable.lookup(name) != NULL) {
        classtable->semant_error(curr_class) << "Attribute " << name << " is an attribute of an inherited class.\n";
        return;
    }

    attribtable.addid(name, new Symbol(type_decl));
}

// Type checking functions

void method_class::CheckFeatureType(ClassTable *classtable) {
    if (classtable->m_classes.find(return_type) == classtable->m_classes.end() && return_type != SELF_TYPE) {
        classtable->semant_error(curr_class->get_filename(), this) << "Undefined return type " << return_type << " in method " << name << ".\n";
    }
    attribtable.enterscope();
    std::set<Symbol> used_names;
    for (int i = formals->first(); formals->more(i); i = formals->next(i)) {
        Symbol name = formals->nth(i)->GetName();
        if (used_names.find(name) != used_names.end()) {
            classtable->semant_error(curr_class->get_filename(), formals->nth(i)) << "Formal parameter " << formals->nth(i)->GetName() << " is multiply defined.\n";
        } else {
            used_names.insert(name);
        }

        Symbol type = formals->nth(i)->GetType();
        if (classtable->m_classes.find(type) == classtable->m_classes.end()) {
            classtable->semant_error(curr_class->get_filename(), formals->nth(i)) << "Formal parameter " << formals->nth(i)->GetName()  << " cannot have type " << type << ".\n";
        }
        if (formals->nth(i)->GetName() == self) {
            classtable->semant_error(curr_class->get_filename(), formals->nth(i)) << "'self' cannot be the name of a formal parameter.\n";
        }
        attribtable.addid(formals->nth(i)->GetName(), new Symbol(formals->nth(i)->GetType()));
    }
    
    Symbol expr_type = expr->CheckExprType(classtable);
    if (classtable->CheckInheritance(return_type, expr_type) == false) {
        classtable->semant_error(curr_class->get_filename(), this) << "Inferred return type " << expr_type << " of method " << name << " does not conform to declared return type " << return_type << ".\n";
    }
    attribtable.exitscope();
}

void attr_class::CheckFeatureType(ClassTable *classtable) {
    init->CheckExprType(classtable);
}

Symbol assign_class::CheckExprType(ClassTable *classtable) {
    Symbol* lvalue_type = attribtable.lookup(name);
    Symbol rvalue_type = expr->CheckExprType(classtable);
    if (name == self){
        classtable->semant_error(curr_class->get_filename(), this) << "Cannot assign to 'self'.\n";
        type = Object;
        return type;
    }
    if (lvalue_type == NULL) {
        classtable->semant_error(curr_class->get_filename(), this) << "Assignment to undeclared variable " << name << ".\n";
        type = Object;
        return type;
    }
    if (classtable->CheckInheritance(*lvalue_type, rvalue_type) == false) {
        classtable->semant_error(curr_class->get_filename(), this) << "Type " << rvalue_type << " of assigned expression does not conform to declared type "
        << *lvalue_type <<  " of identifier " << name <<".\n";
        type = Object;
        return type;
    }
    type = rvalue_type;
    return type;
}

Symbol static_dispatch_class::CheckExprType(ClassTable *classtable) {
    bool error = false;

    Symbol expr_class = expr->CheckExprType(classtable);

    if (classtable->CheckInheritance(type_name, expr_class) == false) {
        error = true;
        classtable->semant_error(curr_class->get_filename(), this) << "Expression type " << expr_class << " does not conform to declared static dispatch type " << type_name << ".\n";
    }

    std::list<Symbol> path = classtable->GetInheritancePath(type_name);
    method_class* method = NULL;
    for (std::list<Symbol>::iterator iter = path.begin(); iter != path.end(); ++iter) {
        if ((method = methodtables[*iter].lookup(name)) != NULL) {
            break;
        }
    }

    if (method == NULL) {
        error = true;
        classtable->semant_error(curr_class->get_filename(), this) << "Dispatch to undefined method " << name << ".\n";
    }

    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        Symbol actual_type = actual->nth(i)->CheckExprType(classtable);
        if (method != NULL) {
            Symbol formal_type = method->GetFormals()->nth(i)->GetType();
            if (classtable->CheckInheritance(formal_type, actual_type) == false) {
                classtable->semant_error(curr_class->get_filename(), this) << "In call of method " << name << ", type " << actual_type << " of parameter " << method->GetFormals()->nth(method->GetFormals()->first())->GetName() << " does not conform to declared type " << formal_type << ".\n";
                error = true;
            }
        }
    }

    if (error) {
        type = Object;
    } else {
        type = method->GetType();
        if (type == SELF_TYPE) {
            type = type_name;
        }
    }

    return type;
}

Symbol dispatch_class::CheckExprType(ClassTable *classtable) {
    bool error = false;

    Symbol expr_type = expr->CheckExprType(classtable);

    std::list<Symbol> path = classtable->GetInheritancePath(expr_type);
    method_class* method = NULL;
    for (std::list<Symbol>::iterator iter = path.begin(); iter != path.end(); ++iter) {
        if ((method = methodtables[*iter].lookup(name)) != NULL) {
            break;
        }
    }

    if (method == NULL) {
        error = true;
        classtable->semant_error(curr_class->get_filename(), this) << "Dispatch to undefined method " << name << ".\n";
    }

    for (int i = actual->first(); actual->more(i); i = actual->next(i)) {
        Symbol actual_type = actual->nth(i)->CheckExprType(classtable);
        if (method != NULL) {
            Symbol formal_type = method->GetFormals()->nth(i)->GetType();
            if (classtable->CheckInheritance(formal_type, actual_type) == false) {
                classtable->semant_error(curr_class->get_filename(), this) << "In call of method " << name << ", type " << actual_type << " of parameter " << method->GetFormals()->nth(method->GetFormals()->first())->GetName() << " does not conform to declared type " << formal_type << ".\n";
                error = true;
            }
        }
    }

    if (error) {
        type = Object;
    } else {
        type = method->GetType();
        if (type == SELF_TYPE) {
            type = expr_type;
        }
    }

    return type;
}

Symbol cond_class::CheckExprType(ClassTable *classtable) {
    if (pred->CheckExprType(classtable) != Bool) {
        classtable->semant_error(curr_class->get_filename(), this) << "Predicate of 'if' does not have type Bool.\n";
    }

    Symbol then_type = then_exp->CheckExprType(classtable);
    Symbol else_type = else_exp->CheckExprType(classtable);

    if (else_type == No_type) {
        type = then_type;
    } else {
        type = classtable->FindCommonAncestor(then_type, else_type);
    }
    return type;
}

Symbol loop_class::CheckExprType(ClassTable *classtable) {
    if (pred->CheckExprType(classtable) != Bool) {
        classtable->semant_error(curr_class->get_filename(), this) << "Loop condition does not have type Bool.\n";
    }
    body->CheckExprType(classtable);
    type = Object;
    return type;
}

Symbol typcase_class::CheckExprType(ClassTable *classtable) {

    Symbol expr_type = expr->CheckExprType(classtable);

    Case branch;
    std::vector<Symbol> branch_types;
    std::vector<Symbol> branch_type_decls;

    for (int i = cases->first(); cases->more(i); i = cases->next(i)) {
        branch = cases->nth(i);
        Symbol branch_type = branch->CheckBranchType(classtable);
        branch_types.push_back(branch_type);
        branch_type_decls.push_back(((branch_class *)branch)->GetTypeDecl());
    }

    for (int i = 0; i < branch_types.size() - 1; ++i) {
        for (int j = i + 1; j < branch_types.size(); ++j) {
            if (branch_type_decls[i] == branch_type_decls[j]) {
                classtable->semant_error(curr_class->get_filename(), this) << "Duplicate branch "<< branch_type_decls[i] <<" in case statement.\n";
            }
        }
    }

    type = branch_types[0];
    for (int i = 1; i < branch_types.size(); ++i) {
        type = classtable->FindCommonAncestor(type, branch_types[i]);
    }
    return type;
}

Symbol branch_class::CheckBranchType(ClassTable *classtable) {
    attribtable.enterscope();

    attribtable.addid(name, new Symbol(type_decl));
    Symbol type = expr->CheckExprType(classtable);

    attribtable.exitscope();

    return type;
}

Symbol block_class::CheckExprType(ClassTable *classtable) {
    for (int i = body->first(); body->more(i); i = body->next(i)) {
        type = body->nth(i)->CheckExprType(classtable);
    }
    return type;
}

Symbol let_class::CheckExprType(ClassTable *classtable) {
    if (identifier == self) {
        classtable->semant_error(curr_class->get_filename(), this) << "'self' cannot be bound in a 'let' expression.\n";
    }

    attribtable.enterscope();
    attribtable.addid(identifier, new Symbol(type_decl));

    Symbol init_type = init->CheckExprType(classtable);
    if (init_type != No_type) {
        if (classtable->CheckInheritance(type_decl, init_type) == false) {
            classtable->semant_error(curr_class->get_filename(), this)  << "Inferred type " << init_type 
            << " of initialization of " << identifier
            << " does not conform to identifier's declared type " << type_decl << ".\n";
            // Inferred type A of initialization of x does not conform to identifier's declared type B.
        }
    }

    type = body->CheckExprType(classtable);
    attribtable.exitscope();
    return type;
}

Symbol plus_class::CheckExprType(ClassTable *classtable) {
    Symbol e1_type = e1->CheckExprType(classtable);
    Symbol e2_type = e2->CheckExprType(classtable);
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(curr_class->get_filename(), this) << "non-Int arguments: " << e1_type << " + " << e2_type << "\n";
        type = Object;
    } else {
        type = Int;
    }
    return type;
}

Symbol sub_class::CheckExprType(ClassTable *classtable) {
    Symbol e1_type = e1->CheckExprType(classtable);
    Symbol e2_type = e2->CheckExprType(classtable);
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(curr_class->get_filename(), this) << "non-Int arguments: " << e1_type << " - " << e2_type << "\n";
        type = Object;
    } else {
        type = Int;
    }
    return type;
}

Symbol mul_class::CheckExprType(ClassTable *classtable) {
    Symbol e1_type = e1->CheckExprType(classtable);
    Symbol e2_type = e2->CheckExprType(classtable);
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(curr_class->get_filename(), this) << "non-Int arguments: " << e1_type << " * " << e2_type << "\n";
        type = Object;
    } else {
        type = Int;
    }
    return type;
}

Symbol divide_class::CheckExprType(ClassTable *classtable) {
    Symbol e1_type = e1->CheckExprType(classtable);
    Symbol e2_type = e2->CheckExprType(classtable);
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(curr_class->get_filename(), this) << "non-Int arguments: " << e1_type << " / " << e2_type << "\n";
        type = Object;
    } else {
        type = Int;
    }
    return type;
}

Symbol neg_class::CheckExprType(ClassTable *classtable) {
    Symbol e1_type = e1->CheckExprType(classtable);
    if (e1_type != Int) {
        classtable->semant_error(curr_class->get_filename(), this) << "Argument of '~' has type " << e1_type << " instead of Int.\n";
        type = Object;
    } else {
        type = Int;
    }
    return type;
}

Symbol lt_class::CheckExprType(ClassTable *classtable) {
    Symbol e1_type = e1->CheckExprType(classtable);
    Symbol e2_type = e2->CheckExprType(classtable);
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(curr_class->get_filename(), this) << "non-Int arguments: " << e1_type << " < " << e2_type << "\n";
        type = Object;
    } else {
        type = Bool;
    }
    return type;
}

Symbol eq_class::CheckExprType(ClassTable *classtable) {
    Symbol e1_type = e1->CheckExprType(classtable);
    Symbol e2_type = e2->CheckExprType(classtable);
    if (e1_type == Int || e2_type == Int || e1_type == Bool || e2_type == Bool || e1_type == Str || e2_type == Str) {
        if (e1_type != e2_type) {
            classtable->semant_error(curr_class->get_filename(), this) << "Illegal comparison with a basic type.\n";
            type = Object;
        } else {
            type = Bool;
        }
    } else {
        type = Bool;
    }
    return type;
}

Symbol leq_class::CheckExprType(ClassTable *classtable) {
    Symbol e1_type = e1->CheckExprType(classtable);
    Symbol e2_type = e2->CheckExprType(classtable);
    if (e1_type != Int || e2_type != Int) {
        classtable->semant_error(curr_class->get_filename(), this) << "non-Int arguments: " << e1_type << " <= " << e2_type << "\n";
        type = Object;
    } else {
        type = Bool;
    }
    return type;
}

Symbol comp_class::CheckExprType(ClassTable *classtable) {
    Symbol e1_type = e1->CheckExprType(classtable);
    if (e1_type != Bool) {
        classtable->semant_error(curr_class->get_filename(), this) << "Argument of 'not' has type " << e1_type << " instead of Bool.\n";
        type = Object;
    } else {
        type = Bool;
    }
    return type;
}

Symbol int_const_class::CheckExprType(ClassTable *classtable) {
    type = Int;
    return type;
}

Symbol bool_const_class::CheckExprType(ClassTable *classtable) {
    type = Bool;
    return type;
}

Symbol string_const_class::CheckExprType(ClassTable *classtable) {
    type = Str;
    return type;
}

Symbol new__class::CheckExprType(ClassTable *classtable) {
    if (type_name != SELF_TYPE && classtable->m_classes.find(type_name) == classtable->m_classes.end()) {
        classtable->semant_error(curr_class->get_filename(), this) << "'new' used with undefined class " << type_name << ".\n";
    }
    type = type_name;
    return type;
}

Symbol isvoid_class::CheckExprType(ClassTable *classtable) {
    e1->CheckExprType(classtable);
    type = Bool;
    return type;
}

Symbol no_expr_class::CheckExprType(ClassTable *classtable) {
    return No_type;
}


Symbol object_class::CheckExprType(ClassTable *classtable) {
    if (name == self) {
        type = SELF_TYPE;
        return type;
    }

    Symbol* found_type = attribtable.lookup(name);
    if (found_type == NULL) {
        classtable->semant_error(curr_class->get_filename(), this) << "Undeclared identifier " << name <<".\n";
        type = Object;
    } else {
        type = *found_type;
    }
    
    return type;
}

void ClassTable::ConstructMethodtables(){
    for (std::map<Symbol, Class_>::iterator iter = this->m_classes.begin(); iter != this->m_classes.end(); ++iter) {

        Symbol class_name = iter->first;
        methodtables[class_name].enterscope();
        Features curr_features = this->m_classes[class_name]->GetFeatures();
        for (int j = curr_features->first(); curr_features->more(j); j = curr_features->next(j)) {
             Feature curr_feature = curr_features->nth(j);
             curr_feature->AddMethodToTable(class_name, this);
        }
    }
}

void ClassTable::MethodChecking(){
    for (std::map<Symbol, Class_>::iterator iter = this->m_classes.begin(); iter != this->m_classes.end(); ++iter) {
        
        Symbol class_name = iter->first;
        curr_class = this->m_classes[class_name];

        Features curr_features = this->m_classes[class_name]->GetFeatures();

        for (int j = curr_features->first(); curr_features->more(j); j = curr_features->next(j)) {
            
            Feature curr_method = curr_features->nth(j);

            if (curr_method->IsMethod() == false) {
                continue;
            }

            Formals curr_formals = ((method_class*)(curr_method))->GetFormals();
            
            std::list<Symbol> path = this->GetInheritancePath(class_name);
            // We are checking every method with the same name in the ancestors
            for (std::list<Symbol>::reverse_iterator iter = path.rbegin(); iter != path.rend(); ++iter) {
                
                Symbol ancestor_name = *iter;
                method_class* method = methodtables[ancestor_name].lookup(curr_method->GetName());
                
                if (method != NULL) {
                    // A method is found.
                    Formals formals = method->GetFormals();

                    int k1 = formals->first(), k2 = curr_formals->first();
                    for (; formals->more(k1) && curr_formals->more(k2); k1 = formals->next(k1), k2 = formals->next(k2)) {
                        if (formals->nth(k1)->GetType() != curr_formals->nth(k2)->GetType()) {
                            this->semant_error(this->m_classes[class_name]->get_filename(),method) << "In redefined method " << curr_method->GetName() << ", parameter type " << curr_formals->nth(k2)->GetType() << " is different from original type " << formals->nth(k1)->GetType() << ".\n";
                        }
                    }

                    if (formals->more(k1) || curr_formals->more(k2)) {
                        this->semant_error(this->m_classes[class_name]->get_filename(),method) << "Incompatible number of formal parameters in redefined method foo.\n";
                    }
                }
            }
        }
    }

    // Check if we can find class Main.
    if (m_classes.find(Main) == m_classes.end()) {
        semant_error() << "Class Main is not defined.\n";
    }
}


void ClassTable::TypeChecking(Classes classes){
    for (int i = classes->first(); classes->more(i); i = classes->next(i)) {
        curr_class = classes->nth(i);
        std::list<Symbol> path = this->GetInheritancePath(curr_class->GetName());
        for (std::list<Symbol>::iterator iter = path.begin(); iter != path.end(); iter++) {
            curr_class = this->m_classes[*iter];
            Features curr_features = curr_class->GetFeatures();
            attribtable.enterscope();
            for (int j = curr_features->first(); curr_features->more(j); j = curr_features->next(j)) {
                Feature curr_feature = curr_features->nth(j);
                curr_feature->AddAttribToTable(curr_class->GetName(), this);
            }
        }
        
        curr_class = classes->nth(i);
        Features curr_features = curr_class->GetFeatures();
        for (int j = curr_features->first(); curr_features->more(j); j = curr_features->next(j)) {
            Feature curr_feature = curr_features->nth(j);
            curr_feature->CheckFeatureType(this);
        }

        for (int j = 0; j < path.size(); ++j) {
            attribtable.exitscope();
        }
    }
}


void program_class::semant() {
    initialize_constants();

    // ClassTable constructor may do some semantic analysis
    ClassTable *classtable = new ClassTable(classes);

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }

    classtable->ConstructMethodtables();
    classtable->MethodChecking();
    classtable->TypeChecking(classes);

    if (classtable->errors()) {
        cerr << "Compilation halted due to static semantic errors." << endl;
        exit(1);
    }

}
