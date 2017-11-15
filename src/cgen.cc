//**************************************************************
//
// Code generator SKELETON
//
// Read the comments carefully and add code to build an LLVM program 
//**************************************************************

#define EXTERN
#include "cgen.h"
#include <string>
#include <sstream>

using namespace std;
// forgive me for using std, otherwise me IDE keeps yielling at me
// clion has a bug with vector type decleartion
extern int cgen_debug;
unordered_map<string, string> string_const_map;
unordered_map<string, CgenNode*> name_class_map;

//////////////////////////////////////////////////////////////////////
//
// Symbols
//
// For convenience, a large number of symbols are predefined here.
// These symbols include the primitive type and method names, as well
// as fixed names used by the runtime system.  Feel free to add your
// own definitions as you see fit.
//
//////////////////////////////////////////////////////////////////////
EXTERN Symbol 
	// required classes
	Object,
	IO,
	String,
	Int,
	Bool,
	Main,

	// class methods
	cool_abort,
	type_name,
	cool_copy,
	out_string,
	out_int,
	in_string,
	in_int,
	length,
	concat,
	substr,

	// class members
	val,

	// special symbols
	No_class,    // symbol that can't be the name of any user-defined class
	No_type,     // If e : No_type, then no code is generated for e.
	SELF_TYPE,   // Special code is generated for new SELF_TYPE.
	self,        // self generates code differently than other references

	// extras
	arg,
	arg2,
	prim_string,
	prim_int,
	prim_bool;


//********************************************************
//
// PREDEFINED FUNCTIONS:
//
// The following functions are already coded, you should
// not need to modify them, although you may if necessary.
//
//********************************************************

//
// Initializing the predefined symbols.
//
static void initialize_constants(void)
{
	Object      = idtable.add_string("Object");
	IO          = idtable.add_string("IO");
	String      = idtable.add_string("String");
	Int         = idtable.add_string("Int");
	Bool        = idtable.add_string("Bool");
	Main        = idtable.add_string("Main");

	cool_abort  = idtable.add_string("abort");
	type_name   = idtable.add_string("type_name");
	cool_copy   = idtable.add_string("copy");
	out_string  = idtable.add_string("out_string");
	out_int     = idtable.add_string("out_int");
	in_string   = idtable.add_string("in_string");
	in_int      = idtable.add_string("in_int");
	length      = idtable.add_string("length");
	concat      = idtable.add_string("concat");
	substr      = idtable.add_string("substr");

	val         = idtable.add_string("val");

	No_class    = idtable.add_string("_no_class");
	No_type     = idtable.add_string("_no_type");
	SELF_TYPE   = idtable.add_string("SELF_TYPE");
	self        = idtable.add_string("self");

	arg         = idtable.add_string("arg");
	arg2        = idtable.add_string("arg2");
	prim_string = idtable.add_string("sbyte*");
	prim_int    = idtable.add_string("int");
	prim_bool   = idtable.add_string("bool");
}

//*********************************************************
//
// Define method for code generation
//
// This is the method called by the compiler driver
// `cgtest.cc'. cgen takes an `ostream' to which the assembly will be
// emitted, and it passes this and the class list of the
// code generator tree to the constructor for `CgenClassTable'.
// That constructor performs all of the work of the code
// generator.
//
//*********************************************************
void program_class::cgen(ostream &os) 
{
	initialize_constants();
	class_table = new CgenClassTable(classes,os);
}


// Create definitions for all String constants
void StrTable::code_string_table(ostream& s, CgenClassTable* ct)
{
	for (List<StringEntry> *l = tbl; l; l = l->tl()) {
		l->hd()->code_def(s, ct);
	}
}

// Create definitions for all Int constants
void IntTable::code_string_table(ostream& s, CgenClassTable* ct)
{
	for (List<IntEntry> *l = tbl; l; l = l->tl()) {
		l->hd()->code_def(s, ct);
	}
}

//
// Sets up declarations for extra functions needed for code generation
// You should not need to modify this code for MP2.1
//
void CgenClassTable::setup_external_functions()
{
	ValuePrinter vp;
	// setup function: external int strcmp(sbyte*, sbyte*)
	op_type i32_type(INT32), i8ptr_type(INT8_PTR), vararg_type(VAR_ARG);
	vector<op_type> strcmp_args;
	strcmp_args.push_back(i8ptr_type);
	strcmp_args.push_back(i8ptr_type);	
	vp.declare(*ct_stream, i32_type, "strcmp", strcmp_args); 

	// setup function: external int printf(sbyte*, ...)
	vector<op_type> printf_args;
	printf_args.push_back(i8ptr_type);
	printf_args.push_back(vararg_type);
	vp.declare(*ct_stream, i32_type, "printf", printf_args);

	// setup function: external void abort(void)
	op_type void_type(VOID);
	vector<op_type> abort_args;
	vp.declare(*ct_stream, void_type, "abort", abort_args);

	// setup function: external i8* malloc(i32)
	vector<op_type> malloc_args;
	malloc_args.push_back(i32_type);
	vp.declare(*ct_stream, i8ptr_type, "malloc", malloc_args);

#ifdef MP3
	//ADD CODE HERE
	//Setup external functions for built in object class functions
    //ADD CODE HERE
    op_type objPtr_type("Object*"), strPtr_type("String*"),
            ioPtr_type("IO*"), intObjPtr_type("Int*"), boolObj_ptr_type("Bool*");
    // declare %Object* @Object_new()
    vector<op_type> obj_new_args;
    vp.declare(*ct_stream, objPtr_type, "Object_new", obj_new_args);

    // declare %Object* @Object_abort(%Object*)
    vector<op_type> obj_abort_args;
    obj_abort_args.push_back(objPtr_type);
    vp.declare(*ct_stream, objPtr_type, "Object_abort", obj_abort_args);

    // declare %String* @Object_type_name(%Object*)
    vector<op_type> obj_type_args;
    obj_type_args.push_back(objPtr_type);
    vp.declare(*ct_stream, strPtr_type, "Object_type_name", obj_type_args);

    // declare %Object* @Object_copy(%Object*)
    vector<op_type> obj_copy_args;
    obj_copy_args.push_back(objPtr_type);
    vp.declare(*ct_stream, objPtr_type, "Object_copy", obj_copy_args);

    // declare %IO* @IO_new()
    vector<op_type> io_new_args;
    vp.declare(*ct_stream, ioPtr_type, "IO_new", io_new_args);

    // declare %IO* @IO_out_string(%IO*, %String*)
    vector<op_type> io_out_string_args;
    io_out_string_args.push_back(ioPtr_type);
    io_out_string_args.push_back(strPtr_type);
    vp.declare(*ct_stream, ioPtr_type, "IO_out_string", io_out_string_args);

    // declare %IO* @IO_out_int(%IO*, i32)
    vector<op_type> io_out_int_args;
    io_out_int_args.push_back(ioPtr_type);
    io_out_int_args.push_back(i32_type);
    vp.declare(*ct_stream, ioPtr_type, "IO_out_int", io_out_int_args);

    // declare %String* @IO_in_string(%IO*)
    vector<op_type> io_in_string_args;
    io_in_string_args.push_back(ioPtr_type);
    vp.declare(*ct_stream, strPtr_type, "IO_in_string", io_in_string_args);

    // declare i32 @IO_in_int(%IO*)
    vector<op_type> io_in_int_args;
    io_in_int_args.push_back(ioPtr_type);
    vp.declare(*ct_stream, i32_type, "IO_in_int", io_in_int_args);

    // declare %String* @String_new()
    vector<op_type> string_new_args;
    vp.declare(*ct_stream, strPtr_type, "String_new", string_new_args);

    // declare i32 @String_length(%String*)
    vector<op_type> str_len_args;
    str_len_args.push_back(strPtr_type);
    vp.declare(*ct_stream, i32_type, "String_length", str_len_args);

    // declare %String* @String_concat(%String*, %String*)
    vector<op_type> str_concat_args;
    str_concat_args.push_back(strPtr_type);
    str_concat_args.push_back(strPtr_type);
    vp.declare(*ct_stream, strPtr_type, "String_concat", str_concat_args);

    // declare %String* @String_substr(%String*, i32, i32)
    vector<op_type> str_substr_args;
    str_substr_args.push_back(strPtr_type);
    str_substr_args.push_back(i32_type);
    str_substr_args.push_back(i32_type);
    vp.declare(*ct_stream, strPtr_type, "String_substr", str_substr_args);

    // declare %Int* @Int_new()
    vector<op_type> int_new_args;
    vp.declare(*ct_stream, intObjPtr_type, "Int_new", int_new_args);

    // declare void @Int_init(%Int*, i32)
    vector<op_type> int_init_args;
    int_init_args.push_back(intObjPtr_type);
    int_init_args.push_back(i32_type);
    vp.declare(*ct_stream, void_type, "Int_init", int_init_args);

    // declare %Bool* @Bool_new()
    vector<op_type> bool_new_args;
    vp.declare(*ct_stream, boolObj_ptr_type, "Bool_new", bool_new_args);

    // declare void @Bool_init(%Bool*, i1)
    vector<op_type> bool_init_args;
    bool_init_args.push_back(boolObj_ptr_type);
    bool_init_args.push_back(op_type(INT1));
    vp.declare(*ct_stream, void_type, "Bool_init", bool_init_args);
#endif
}

// Creates AST nodes for the basic classes and installs them in the class list
void CgenClassTable::install_basic_classes()
{
	// The tree package uses these globals to annotate the classes built below.
	curr_lineno = 0;
	Symbol filename = stringtable.add_string("<basic class>");

	//
	// A few special class names are installed in the lookup table but not
	// the class list. Thus, these classes exist, but are not part of the
	// inheritance hierarchy.
	 
	// No_class serves as the parent of Object and the other special classes.
	Class_ noclasscls = class_(No_class,No_class,nil_Features(),filename);
	install_special_class(new CgenNode(noclasscls, CgenNode::Basic, this));
	delete noclasscls;

#ifdef MP3
	// SELF_TYPE is the self class; it cannot be redefined or inherited.
	Class_ selftypecls = class_(SELF_TYPE,No_class,nil_Features(),filename);
	install_special_class(new CgenNode(selftypecls, CgenNode::Basic, this));
	delete selftypecls;
	// 
	// Primitive types masquerading as classes. This is done so we can
	// get the necessary Symbols for the innards of String, Int, and Bool
	//
	Class_ primstringcls = class_(prim_string,No_class,nil_Features(),filename);
	install_special_class(new CgenNode(primstringcls, CgenNode::Basic, this));
	delete primstringcls;
#endif
	Class_ primintcls = class_(prim_int,No_class,nil_Features(),filename);
	install_special_class(new CgenNode(primintcls, CgenNode::Basic, this));
	delete primintcls;
	Class_ primboolcls = class_(prim_bool,No_class,nil_Features(),filename);
	install_special_class(new CgenNode(primboolcls, CgenNode::Basic, this));
	delete primboolcls;
	// 
	// The Object class has no parent class. Its methods are
	//        cool_abort() : Object   aborts the program
	//        type_name() : Str       returns a string representation of class name
	//        copy() : SELF_TYPE      returns a copy of the object
	//
	// There is no need for method bodies in the basic classes---these
	// are already built in to the runtime system.
	//
	Class_ objcls =
		class_(Object, 
		       No_class,
		       append_Features(
		       append_Features(
		       single_Features(method(cool_abort, nil_Formals(), 
		                              Object, no_expr())),
		       single_Features(method(type_name, nil_Formals(),
		                              String, no_expr()))),
		       single_Features(method(cool_copy, nil_Formals(), 
		                              SELF_TYPE, no_expr()))),
		       filename);
	install_class(new CgenNode(objcls, CgenNode::Basic, this));
	delete objcls;

//
// The Int class has no methods and only a single attribute, the
// "val" for the integer. 
//
	Class_ intcls=
		class_(Int, 
		       Object,
		       single_Features(attr(val, prim_int, no_expr())),
		       filename);
	install_class(new CgenNode(intcls, CgenNode::Basic, this));
	delete intcls;

//
// Bool also has only the "val" slot.
//
	Class_ boolcls=
		class_(Bool,  
		       Object, 
		       single_Features(attr(val, prim_bool, no_expr())),
		       filename);
	install_class(new CgenNode(boolcls, CgenNode::Basic, this));
	delete boolcls;

#ifdef MP3
//
// The class String has a number of slots and operations:
//       val                                  the string itself
//       length() : Int                       length of the string
//       concat(arg: Str) : Str               string concatenation
//       substr(arg: Int, arg2: Int): Str     substring
//       
	Class_ stringcls =
		class_(String, 
		       Object,
		       append_Features(
		       append_Features(
		       append_Features(
		       single_Features(attr(val, prim_string, no_expr())),
		       single_Features(method(length, nil_Formals(),
		                              Int, no_expr()))),
		       single_Features(method(concat,
		                              single_Formals(formal(arg, String)),
		                              String,
		                              no_expr()))),
		       single_Features(method(substr, 
		                              append_Formals(
		                                 single_Formals(formal(arg, Int)), 
		                                 single_Formals(formal(arg2, Int))),
		                              String, 
		                              no_expr()))),
		       filename);
	install_class(new CgenNode(stringcls, CgenNode::Basic, this));
	delete stringcls;
#endif

#ifdef MP3
// 
// The IO class inherits from Object. Its methods are
//        out_string(Str) : SELF_TYPE          writes a string to the output
//        out_int(Int) : SELF_TYPE               "    an int    "  "     "
//        in_string() : Str                    reads a string from the input
//        in_int() : Int                         "   an int     "  "     "
//
	Class_ iocls =
		class_(IO,
		       Object,
		       append_Features(
		       append_Features(
		       append_Features(
		       single_Features(method(out_string,
		                              single_Formals(formal(arg, String)),
		                              SELF_TYPE, no_expr())),
		       single_Features(method(out_int, single_Formals(formal(arg, Int)),
		                              SELF_TYPE, no_expr()))),
		       single_Features(method(in_string, nil_Formals(), String,
		                              no_expr()))),
		       single_Features(method(in_int, nil_Formals(), Int, no_expr()))),
		       filename);
	install_class(new CgenNode(iocls, CgenNode::Basic, this));
	delete iocls;
#endif
}

//
// install_classes enters a list of classes in the symbol table.
//
void CgenClassTable::install_classes(Classes cs)
{
	for (int i = cs->first(); cs->more(i); i = cs->next(i)) {
		install_class(new CgenNode(cs->nth(i),CgenNode::NotBasic,this));
	}
}

// 
// Add this CgenNode to the class list and the lookup table
// 
void CgenClassTable::install_class(CgenNode *nd)
{
	Symbol name = nd->get_name();

	if (probe(name))
		return;

	// The class name is legal, so add it to the list of classes
	// and the symbol table.
	nds = new List<CgenNode>(nd,nds);
	addid(name,nd);
}

// 
// Add this CgenNode to the special class list and the lookup table
// 
void CgenClassTable::install_special_class(CgenNode *nd)
{
	Symbol name = nd->get_name();

	if (probe(name))
		return;

	// The class name is legal, so add it to the list of special classes
	// and the symbol table.
	special_nds = new List<CgenNode>(nd, special_nds);
	addid(name,nd);
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
void CgenClassTable::set_relations(CgenNode *nd)
{
	CgenNode *parent_node = probe(nd->get_parent());
	nd->set_parentnd(parent_node);
	parent_node->add_child(nd);
}

// Get the root of the class tree.
CgenNode *CgenClassTable::root()
{
	return probe(Object);
}

//////////////////////////////////////////////////////////////////////
//
// Special-case functions used for the method Int Main::main() for
// MP2-1 only.
//
//////////////////////////////////////////////////////////////////////

#ifndef MP3

CgenNode* CgenClassTable::getMainmain(CgenNode* c)
{
	if (c && ! c->basic())
		return c;                   // Found it!

	List<CgenNode> *children = c->get_children();
	for (List<CgenNode> *child = children; child; child = child->tl()) {
		if (CgenNode* foundMain = this->getMainmain(child->hd()))
			return foundMain;   // Propagate it up the recursive calls
	}

	return 0;                           // Make the recursion continue
}

#endif

//-------------------------------------------------------------------
//
// END OF PREDEFINED FUNCTIONS
//
//-------------------------------------------------------------------


///////////////////////////////////////////////////////////////////////////////
//
// coding string, int, and boolean constants
//
// Cool has three kinds of constants: strings, ints, and booleans.
// This section defines code generation for each type.
//
// All string constants are listed in the global "stringtable" and have
// type stringEntry.  stringEntry methods are defined both for string
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
// Create global definitions for constant Cool objects
//
void CgenClassTable::code_constants()
{
#ifdef MP3

	// generate code for string obj
    CgenEnvironment *env = new CgenEnvironment(*(this->ct_stream), root());
    ValuePrinter vp(*(env->cur_stream));

    // as it is in reverse order just to make it looks like reference
    int i;
    for (i = stringtable.first(); stringtable.more(i); i = stringtable.next(i)) {}

    for (int x = i - 1; x >= stringtable.first(); x--) {
        Symbol str_from_table = stringtable.lookup(x);

		// code for the string object's content
//		@str.1 = internal constant [14 x i8] c"<basic class>\00", align 1
        op_arr_type op_type_array(INT8, string(str_from_table->get_string()).length() + 1);
        const_value str_obj_content(op_type_array, str_from_table->get_string(), true);
        string str_obj_content_name = "str.";
        str_obj_content_name = str_obj_content_name + to_string(x);
        vp.init_constant(str_obj_content_name, str_obj_content);


		// code for string object
//		@String.1 = constant %String {
//			%String_vtable* @String_vtable_prototype,
//			i8* getelementptr ([14 x i8]* @str.1, i32 0, i32 0)
//		}

		// types for struct, use i8_ptr as last to give value printer a hint for getelementptr
        vector<op_type> struct_field_types;
		// ptr to str vtable
        op_type str_vtab_ptr("String_vtable*");
        struct_field_types.push_back(str_vtab_ptr);
		// ptr to content addr
        op_type i8_ptr_type(INT8_PTR);
        struct_field_types.push_back(i8_ptr_type);

		// valuse for struct
		// vatbale proto
        vector<const_value> struct_field_values;
        const_value str_vtab_proto_const(str_vtab_ptr, string("@String_vtable_prototype"), false);
        struct_field_values.push_back(str_vtab_proto_const);
		//getelement
        string content_ptr_string = "@" + str_obj_content_name;
        const_value str_content(op_type_array, content_ptr_string, false);
        struct_field_values.push_back(str_content);

		// generate the actual code
        string const_str_objt_name = "String." + to_string(x) ;
        global_value string_global(op_type("String"), const_str_objt_name);
        vp.init_struct_constant(string_global, struct_field_types, struct_field_values);

		// add string obj to global map for future indexing
		string_const_map[str_from_table->get_string()] = const_str_objt_name;

	}
#endif
}

// generate code to define a global string constant
void StringEntry::code_def(ostream& s, CgenClassTable* ct)
{
#ifdef MP3
	// ADD CODE HERE
#endif
}

// generate code to define a global int constant
void IntEntry::code_def(ostream& s, CgenClassTable* ct)
{
	// Leave this method blank, since we are not going to use global
	// declarations for int constants.
}

//////////////////////////////////////////////////////////////////////////////
//
//  CgenClassTable methods
//
//////////////////////////////////////////////////////////////////////////////

//
// CgenClassTable constructor orchestrates all code generation
//
CgenClassTable::CgenClassTable(Classes classes, ostream& s) 
: nds(0)
{
	if (cgen_debug) std::cerr << "Building CgenClassTable" << endl;
	ct_stream = &s;
	// Make sure we have a scope, both for classes and for constants
	enterscope();

	// Create an inheritance tree with one CgenNode per class.
	install_basic_classes();
	install_classes(classes);
	build_inheritance_tree();

	// First pass
	setup();

	// Second pass
	code_module();
	// Done with code generation: exit scopes
	exitscope();

}

CgenClassTable::~CgenClassTable()
{
}

// The code generation first pass.  Define these two functions to traverse
// the tree and setup each CgenNode
void CgenClassTable::setup()
{
	setup_external_functions();
	setup_classes(root(), 0);
}


void CgenClassTable::setup_classes(CgenNode *c, int depth)
{
	// MAY ADD CODE HERE
	// if you want to give classes more setup information

	c->setup(current_tag++, depth);
	List<CgenNode> *children = c->get_children();
	for (List<CgenNode> *child = children; child; child = child->tl())
		setup_classes(child->hd(), depth + 1);
	
	c->set_max_child(current_tag-1);


	if (cgen_debug)
		std::cerr << "Class " << c->get_name() << " assigned tag "
			<< c->get_tag() << ", max child " << c->get_max_child()
			 << endl;

}


// The code generation second pass. Add code here to traverse the tree and
// emit code for each CgenNode
void CgenClassTable::code_module()
{
	code_constants();

#ifndef MP3
	// This must be after code_module() since that emits constants
	// needed by the code() method for expressions
	CgenNode* mainNode = getMainmain(root());
	mainNode->codeGenMainmain(*ct_stream);
#endif
	code_main();

#ifdef MP3
	code_classes(root());
#else
#endif
}


#ifdef MP3
void CgenClassTable::code_classes(CgenNode *c)
{

	// ADD CODE HERE
    c->code_class();
    List<CgenNode> *children = c->get_children();
    for (List<CgenNode> *child = children; child; child = child->tl())
        code_classes(child->hd());
}
#endif


//
// Create LLVM entry point. This function will initiate our Cool program 
// by generating the code to execute (new Main).main()
//
void CgenClassTable::code_main()
{
    using std::vector;
	// Define a function main that has no parameters and returns an i32
    ValuePrinter vp(*ct_stream);



#ifndef MP3
	// Get the address of the string "Main_main() returned %d\n" using
	// getelementptr
    string printout_format("Main.main() returned %d\n");
    op_arr_type array_type(INT8, printout_format.length()+1);
    const_value strConst(array_type, printout_format, false);
    vp.init_constant(".str", strConst);

    vector<op_type> main_args_types;
    vector<operand> main_args;
    op_type i32_type(INT32);

    vp.define(i32_type, "main", main_args);
    // Define an entry basic block
    vp.begin_block("entry");

    // Call Main_main(). This returns int for phase 1, Object for phase 2
    operand result = vp.call(main_args_types, i32_type, "Main_main", true, main_args);

    op_arr_type array_type2(INT8, printout_format.length()+1);
    global_value str_ptr(array_type2, ".str");
    operand pointer = vp.getelementptr(array_type2, str_ptr,int_value(0), int_value(0), op_type(INT8_PTR));

    // Call printf with the string address of "Main_main() returned %d\n"
    vector<op_type> printf_args_types;
    printf_args_types.push_back(op_type(INT8_PTR));
    printf_args_types.push_back(op_type(VAR_ARG));

    vector<operand> printf_args;
    printf_args.push_back(pointer);
    printf_args.push_back(result);
    // and the return value of Main_main() as its arguments
    vp.call(printf_args_types, i32_type, "printf", true, printf_args);

	// Insert return 0
    vp.ret(int_value(0));

#else
	// Phase 2
    vector<operand> main_args;
    vector<op_type> main_args_types;
    vp.define(op_type(INT32), "main", main_args);

    // entry:
    vp.begin_block("entry");

    // %main.obj = call %Main*()* @Main_new( )
	// make a new main obj
	op_type empty_op_type;
    main_args_types.push_back(empty_op_type);
    operand empty_op;
    main_args.push_back(empty_op);
    op_type mainPtr("Main*");
    operand main_obj(mainPtr, "main.obj");
    vp.call(*ct_stream, main_args_types, "Main_new", true, main_args, main_obj);

    //%main.retval = call %Object*(%Main*) @Main_main( %Main* %main.obj )
    auto main_cgen_node = name_class_map["%Main*"];
    auto main_main_type = main_cgen_node->get_method_type("main");

	// call main_main
    op_type main_ret_type = main_main_type.get_res_type();
    operand main_retval_result(main_ret_type, "main.retval");
    vector<operand> main_retval_args;
    main_retval_args.push_back(main_obj);
    vector<op_type> main_retval_args_types;
    main_retval_args_types.push_back(mainPtr);
    vp.call(*ct_stream, main_retval_args_types, "Main_main", true, main_retval_args, main_retval_result);

    // ret i32 0
    vp.ret(int_value(0));

#endif
    vp.end_define();

}


///////////////////////////////////////////////////////////////////////
//
// CgenNode methods
//
///////////////////////////////////////////////////////////////////////

CgenNode::CgenNode(Class_ nd, Basicness bstatus, CgenClassTable *ct)
: class__class((const class__class &) *nd), 
  parentnd(0), children(0), basic_status(bstatus), class_table(ct), tag(-1)
{ 
	// ADD CODE HERE
}

void CgenNode::add_child(CgenNode *n)
{
	children = new List<CgenNode>(n,children);
}

void CgenNode::set_parentnd(CgenNode *p)
{
	assert(parentnd == NULL);
	assert(p != NULL);
	parentnd = p;
}

//
// Class setup.  You may need to add parameters to this function so that
// the classtable can provide setup information (such as the class tag
// that should be used by this class).  
// 
// Things that setup should do:
//  - layout the features of the class
//  - create the types for the class and its vtable
//  - create global definitions used by the class such as the class vtable
//
void CgenNode::setup(int tag, int depth)
{
    this->tag = tag;
#ifdef MP3
    // lay out features and add them to temp vtable recordes
	// I use a temp vtable to record those non default functions so that deafult function won't be inherited
    layout_features();

	// inherit things from parent
    inherit();

    op_type i32_type(INT32), i1_type(INT1), i8_ptr_type(INT8_PTR);
    op_type new_func_type(std::string(name->get_string()) + std::string("* () "), 1);
    string className = get_type_name();

	// Add default functions

    //    i32,
    //    i32,
    //    i8*,
    //    %Object* () *,
    vtable_types.push_back(i32_type);
    vtable_types.push_back(i32_type);
    vtable_types.push_back(i8_ptr_type);
    vtable_types.push_back(new_func_type);

    //generate the vtable_prototype
    const_value id_const(i32_type, to_string(this->tag), false);
    vtable_protos.push_back(id_const);
    function_names.emplace_back("tag");
    function_loaction["tag"] = 0;

    //i32 ptrtoint (%Object* getelementptr (%Object, %Object* null, i32 1) to i32),
    //A silly hardcoded string seems to be the easiest way to do this
    string ptr_string = "ptrtoint (%" + className + "* getelementptr (%" + className + ",%" + className + "* null, i32 1) to i32)";
    const_value address_const(i32_type, ptr_string, false);
    vtable_protos.push_back(address_const);
    function_names.emplace_back("size");
    function_loaction["size"] = 1;


    //i8* getelementptr ([class.length+1 x i8]* @str.Class, i32 0, i32 0),
    int name_length = (int)className.length() + 1;
    string getElemptr = "getelementptr ([" + to_string(name_length) + " x i8], [" + to_string(name_length) + " x i8]* @str." + className + ", i32 0, i32 0)";
    const_value name_const(i8_ptr_type, getElemptr, false);
    vtable_protos.push_back(name_const);
    function_names.emplace_back("name_ptr");
    function_loaction["name_ptr"] = 2;

    //%Object* () * @Object_new,
    string new_func_string = className + "_new";
    global_value new_func_value(new_func_type, new_func_string);
    const_value new_const(new_func_type, new_func_value.get_name(), false);
    vtable_protos.push_back(new_const);
    function_names.push_back(new_func_string);
    add_function_loaction(new_func_string);
    function_loaction["new_func_string"] = 3;

	// Finish Adding default functions


	// push things in temp to vtable
    for (size_t i = 0; i < temp_vtable_types.size(); i++)
        vtable_types.push_back(temp_vtable_types.at(i));
    for (size_t i = 0; i < temp_vtable_protos.size(); i++) {
        vtable_protos.push_back(temp_vtable_protos.at(i));
    }


	// generating vtable and vtable proto code here so that it looks more like the reference

	// class name string obj
    ValuePrinter vp(*(this->get_classtable()->ct_stream));
    string class_name_string(name->get_string());
    op_arr_type op_type_array(INT8, class_name_string.length() + 1);
    const_value strConst(op_type_array, class_name_string, true);
    vp.init_constant(std::string("str.") + std::string(name->get_string()), strConst);


//	%Main = type {
//			%Main_vtable*,
//			%Beta*
//	}
    op_type vtable_ptr(std::string(name->get_string()) + "_vtable", 1);
	vector<op_type> type_def_vec;
	type_def_vec.push_back(vtable_ptr);
	for (size_t i = 0; i < attr_types.size(); i++){
		type_def_vec.push_back(attr_types[i]);
	}
	vp.type_define(name->get_string(), type_def_vec);


//	%Main_vtable = type {
//			i32,
//			i32,
//			i8*,
//			...
//	}
	vp.type_define(std::string(name->get_string()) + "_vtable", this->vtable_types);

//	@Main_vtable_prototype = constant %Main_vtable {
//				...
//	}
    op_type proto_const_op(string(name->get_string()) + "_vtable");
    global_value prototype_glbl(proto_const_op, std::string(name->get_string()) + std::string("_vtable_prototype"));
    vp.init_struct_constant(prototype_glbl, vtable_types, vtable_protos);

	// add this cgenNode to global map for future convinence
    name_class_map["%" + get_type_name() + "*"] = this;

#endif
}

#ifdef MP3
//
// Class codegen. This should performed after every class has been setup.
// Generate code for each method of the class.
// Class new function is defined here
void CgenNode::code_class()
{
	// No code generation for basic classes. The runtime will handle that.
	if (basic())
		return;

    CgenEnvironment *env = new CgenEnvironment(*(this->get_classtable()->ct_stream), this);
    ValuePrinter vp(*(env->cur_stream));


    //Now Setup the class_new
    op_type class_ptr_type(this->get_type_name() + "*");
    vector<operand> class_args;
    vp.define(class_ptr_type, this->get_type_name() + "_new", class_args);

    // Define an entry basic block
    string classString("entry");
    vp.begin_block(classString);

//    define %Main* @Main_new() {
//
//        entry:
//        %tpm.4 = getelementptr %Main_vtable, %Main_vtable* @Main_vtable_prototype, i32 0, i32 1
//        %tpm.5 = load i32, i32* %tpm.4
//        %tpm.6 = call i8*(i32) @malloc( i32 %tpm.5 )
//        %tpm.7 = bitcast i8* %tpm.6 to %Main*
//        %tpm.8 = getelementptr %Main, %Main* %tpm.7, i32 0, i32 0
//        store %Main_vtable* @Main_vtable_prototype, %Main_vtable** %tpm.8
//        %tpm.9 = alloca %Main*
//        store %Main* %tpm.7, %Main** %tpm.9
//        ret %Main* %tpm.7
//
//        abort:
//        call void @abort(  )
//        unreachable
//    }

//    %tmp.3 = getelementptr %Main_vtable, %Main_vtable* @Main_vtable_prototype, i32 0, i32 1
    op_type class_vtable_ptr_type(this->get_type_name() + "_vtable*");
    global_value class_vtable_ptr_operand(class_vtable_ptr_type, this->get_type_name() + "_vtable_prototype");
    int_value zero_int_op(0);
    int_value one_int_op(1);
    op_type i32_ptr_type(INT32_PTR);

	// get size
    operand vtable_op = vp.getelementptr(
            class_vtable_ptr_operand.get_type().get_deref_type(),
            class_vtable_ptr_operand,
            zero_int_op,
            one_int_op,
            i32_ptr_type);
    //%tmp.9 = load i32* %tmp.8
    operand load_return = vp.load(vtable_op.get_type().get_deref_type(), vtable_op);

    //%tmp.10 = call i8*(i32 )* @malloc( i32 %tmp.9 )
	// malloc space for the new obj
    op_type i8_ptr_type(INT8_PTR);
    vector<op_type> args_types;
    op_type i32_type(INT32);
    args_types.push_back(i32_type);
    vector<operand> args;
    args.push_back(load_return);
    operand call_return = vp.call(
            args_types,
            i8_ptr_type,
            "malloc",
            true,
            args);

    // %tmp.11 = bitcast i8* %tmp.10 to %Main*
	// bitcast to keep llvm happy
    operand bitcast_result = vp.bitcast(call_return, class_ptr_type);
    //store result of bitcast
    env->malloc_result = bitcast_result;

	//   %tpm.8 = getelementptr %Main, %Main* %tpm.7, i32 0, i32 0
	// set vtable ptr
    op_type class_element_ptr_result_type(this->get_type_name() + "_vtable**");
    operand *class_element_ptr_result = new operand(vp.getelementptr(
            bitcast_result.get_type().get_deref_type(),
            bitcast_result,
            zero_int_op,
            zero_int_op,
            class_element_ptr_result_type));
    //store %Main_vtable* @Main_vtable_prototype, %Main_vtable** %tmp.12
    vp.store(class_vtable_ptr_operand, *class_element_ptr_result);

    //%tmp.13 = alloca %Main*
    operand *alloca_result = new operand(vp.alloca_mem(class_ptr_type));

    //store %Main* %tmp.11, %Main** %tmp.13
    vp.store(bitcast_result, *alloca_result);
    env->add_local(self, *alloca_result);

    //Generate attr init code
    env->generating_methods = false;
    int x = features->first();
    while (features->more(x)) {
        features->nth(x)->code(env);
        x = features->next(x);
    }

    //ret %Main* %tmp.11
    vp.ret(bitcast_result);

    //abort:
    //	call void @abort(  )
    //	unreachable
    //}
    vp.begin_block("abort");
    vp.call(op_type(VOID), "abort", true);
    vp.unreachable();
    vp.end_define();
	// end define new function

    //Generate method
    env->generating_methods = true;
    int i = features->first();
    while (features->more(i)) {
        features->nth(i)->code(env);
        i = features->next(i);
    }
}

// Laying out the features involves creating a Function for each method
// and assigning each attribute a slot in the class structure.
void CgenNode::layout_features()
{
	// ADD CODE HERE
    for (int it = features->first(); features->more(it); it = features->next(it)){
        features->nth(it)->layout_feature(this);
    }

}
#else

// 
// code-gen function main() in class Main
//
void CgenNode::codeGenMainmain(std::ostream &o)
{
    using std::vector;
	ValuePrinter vp(o);
	// In Phase 1, this can only be class Main. Get method_class for main().
	assert(std::string(this->name->get_string()) == std::string("Main"));
	method_class* mainMethod = (method_class*) features->nth(features->first());

	// ADD CODE HERE TO GENERATE THE FUNCTION int Mainmain().
	// Generally what you need to do are:
	// -- setup or create the environment, env, for translating this method
	// -- invoke mainMethod->code(env) to translate the method
    CgenEnvironment * env = new CgenEnvironment(o, this);
    mainMethod->code(env);
}

#endif

//
// CgenEnvironment functions
//

//
// Class CgenEnvironment should be constructed by a class prior to code
// generation for each method.  You may need to add parameters to this
// constructor.
//
CgenEnvironment::CgenEnvironment(std::ostream &o, CgenNode *c)
{
	cur_class = c;
	cur_stream = &o;
	var_table.enterscope();
	tmp_count = block_count = ok_count = 0;
	// ADD CODE HERE
}

// Look up a CgenNode given a symbol
CgenNode *CgenEnvironment::type_to_class(Symbol t) {
	return t == SELF_TYPE ? get_class() 
		: get_class()->get_classtable()->lookup(t);
}

// Provided CgenEnvironment methods
// Generate unique string names
std::string CgenEnvironment::new_name() {
	std::stringstream s;
	s << tmp_count++;
	return "tmp." + s.str();
}

std::string CgenEnvironment::new_ok_label() {
	std::stringstream s;
	s << ok_count++;
	return "ok." + s.str();
}
const std::string CgenEnvironment::new_label(const std::string& prefix,
		bool increment) {
	std::string suffix = itos(block_count);
	block_count += increment;
	return prefix + suffix;
}

void CgenEnvironment::add_local(Symbol name, operand &vb) {
	var_table.enterscope();
	var_table.addid(name, &vb);
}

void CgenEnvironment::kill_local() {
	var_table.exitscope();
}


////////////////////////////////////////////////////////////////////////////
//
// APS class methods
//
////////////////////////////////////////////////////////////////////////////

//******************************************************************
//
//   Fill in the following methods to produce code for the
//   appropriate expression.  You may add or remove parameters
//   as you wish, but if you do, remember to change the parameters
//   of the declarations in `cool-tree.handcode.h'.
//
//*****************************************************************

#ifdef MP3
// conform and get_class_tag are only needed for MP3

// conform - If necessary, emit a bitcast or boxing/unboxing operations
// to convert an object to a new type. This can assume the object
// is known to be (dynamically) compatible with the target type.
// It should only be called when this condition holds.
// (It's needed by the supplied code for typecase)
operand conform(operand src, op_type type, CgenEnvironment *env) {
	// ADD CODE HERE (MP3 ONLY)
	if (cgen_debug) {
		cerr << "conforming" << endl;
		cerr << "conforming result type" << src.get_typename() << endl;
		cerr << "conforming target type" << type.get_name() << endl;
	}

    if (src.get_type().is_same_with(type)){
        return src;
    }

    ValuePrinter vp(*env->cur_stream);
    op_type int_obj_type("Int*");
    op_type bool_obj_type("Bool*");
    op_type object_type(OBJ_PTR);
    op_type void_type(VOID);

	//int can be only box to Int or Object. same as Bool

    // box int
    if (src.get_type().is_same_with(op_type(INT32)) && (type.is_same_with(int_obj_type) || type.is_same_with(object_type))){
        operand int_obj = vp.call(int_obj_type, "Int_new", true);
        vp.call(void_type, "Int_init", true, int_obj, src);

		// if target type is object bitcast again
        if (type.is_same_with(object_type)){
            int_obj = vp.bitcast(int_obj, object_type);
        }
        return int_obj;
    }

    // box bool
    if (src.get_type().is_same_with(op_type(INT1)) && (type.is_same_with(bool_obj_type) || type.is_same_with(object_type))){
        operand bool_obj = vp.call(bool_obj_type, "Bool_new", true);
        vp.call(void_type, "Bool_init", true, bool_obj, src);

		// if target type is object bitcast again
		if (type.is_same_with(object_type)){
            bool_obj = vp.bitcast(bool_obj, object_type);
        }
        return bool_obj;
    }

	// I should also deal with object to int. but it seems that's not tested and i am lazy
	// TODO: Support object to int
    // unbox int
    if (src.get_type().is_same_with(int_obj_type) && type.is_same_with(op_type(INT32))){
        operand val_ptr = vp.getelementptr(op_type("Int"), src, int_value(0), int_value(1), op_type(INT32_PTR));
        operand loaded_val = vp.load(op_type(INT32), val_ptr);
        return loaded_val;
    }

    // unbox bool
    if (src.get_type().is_same_with(bool_obj_type) && type.is_same_with(op_type(INT1))){
        operand val_ptr = vp.getelementptr(op_type("Bool"), src, int_value(0), int_value(1), op_type(INT1_PTR));
        operand loaded_val = vp.load(op_type(INT1), val_ptr);
        return loaded_val;
    }

	// if not int or bool then just bitcast
    auto result = vp.bitcast(src, type);
    return result;
}

// Retrieve the class tag from an object record.
// src is the object we need the tag from.
// src_class is the CgenNode for the *static* class of the expression.
// You need to look up and return the class tag for it's dynamic value
operand get_class_tag(operand src, CgenNode *src_cls, CgenEnvironment *env) {
	// ADD CODE HERE (MP3 ONLY)
    ValuePrinter vp(*env->cur_stream);

//    %tmp.6 = getelementptr %Object, %Object* %tmp.4, i32 0, i32 0
//    %tmp.7 = load %Object_vtable*, %Object_vtable** %tmp.6
//    %tmp.8 = getelementptr %Object_vtable, %Object_vtable* %tmp.7, i32 0, i32 0
//    %tmp.9 = load i32, i32* %tmp.8

	// get class name and load vtable ptr ptr
    string class_name = src_cls->get_type_name();
    op_type class_type(src_cls->get_type_name());
    op_type vtable_ptr_ptr_type(class_name+ "_vtable**");
    operand vtable_ptr_ptr = vp.getelementptr(
            class_type,
            src,
            int_value(0),
            int_value(0),
            vtable_ptr_ptr_type);

    operand vtable_ptr = vp.load(vtable_ptr_ptr_type.get_deref_type(), vtable_ptr_ptr);

	// load vtable ptr
    op_type vtable_ptr_type(class_name + "_vtable*");
    op_type i32_ptr_type(INT32_PTR);

	// load tag from vtable, it is always the first one
    operand tag_ptr = vp.getelementptr(vtable_ptr_type.get_deref_type(), vtable_ptr, int_value(0), int_value(0), i32_ptr_type);
    operand tag = vp.load(op_type(INT32), tag_ptr);
    return tag;
}
#endif

//
// Create a method body
// 
void method_class::code(CgenEnvironment *env)
{
	if (cgen_debug) std::cerr << "method" << endl;
	ValuePrinter vp(*env->cur_stream);
    if(!env->generating_methods){
        return;
    }

//	// ADD CODE HERE
//    ValuePrinter vp(*env->cur_stream);
//    vector<operand> method_args;
//
//    vp.define(op_type(INT32), "Main_main", method_args);
//    vp.begin_block("entry");
//    vp.ret(expr->code(env));
//
//    vp.begin_block("abort");
//    vp.call(op_type(VOID), "abort", true);
//    vp.unreachable();
//
//    vp.end_define();

	/*
 * define %Object* @Main_main(%Main* %self) {
    entry:
        %tmp.0 = alloca %Main*
        store %Main* %self, %Main** %tmp.0
        %tmp.1 = load %Main*, %Main** %tmp.0
        %tmp.2 = bitcast %Main* %tmp.1 to %Object*
        ret %Object* %tmp.2

    abort:
        call void @abort(  )
        unreachable
    }
 */
	// get return types and func name
	op_type func_ret_type(env->get_class()->string_to_type(return_type->get_string()));
	string function_name = env->get_class()->get_type_name() + "_" + name->get_string();

	// process args and types
	op_type self_arg_type(env->get_class()->get_type_name(), 1);
	operand self_arg(self_arg_type, "self");
	vector<operand> args;
	args.push_back(self_arg);
    for (int x = formals->first(); formals->more(x); x = formals->next(x)) {
		Formal formali = formals->nth(x);
		op_type formal_type = env->get_class()->string_to_type(formali->get_type_decl()->get_string());
		operand arg(formal_type, formali->get_name()->get_string());
		args.push_back(arg);
	}

	vp.define(func_ret_type, function_name, args);
	vp.begin_block("entry");

    //Store each arg and add to loacl
	for (int i = 0; i < args.size(); i++) {
		//%tmp.0 = alloca %Main*
		// use new to add operand into env local
		operand* alloca_result = new operand(vp.alloca_mem(args[i].get_type()));
		vp.store(args[i], *alloca_result);
		if(i == 0){
			// it is not in formal and is self
			env->add_local(self, *alloca_result);
		} else {
			env->add_local(formals->nth(i - 1)->get_name(), *alloca_result);
		}
	}

	operand result = expr->code(env);
    result = conform(result, func_ret_type, env);

	vp.ret(result);

	//Kill all args
	for (int j = 0; j < args.size(); ++j) {
		env->kill_local();
	}

    vp.begin_block("abort");
	vp.call(op_type(VOID), "abort", true);
	vp.unreachable();

	vp.end_define();
}

//
// Codegen for expressions.  Note that each expression has a value.
//

operand assign_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "assign" << endl;
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
	ValuePrinter vp(*env->cur_stream);

	operand rfh = expr->code(env);
    string object_name = name->get_string();
    operand *target = env->lookup(name);

    if(target == NULL){
        // if the target is not local then load it from class's attrs to store
        operand *self_op = env->lookup(self);
        operand main_ptr = vp.load(self_op->get_type().get_deref_type(), *self_op);
        int attr_offset = env->get_class()->attr_loaction[name->get_string()];

        // -1 as vtables pointer is not in it
        auto obj_type = env->get_class()->attr_types[attr_offset - 1];
        operand obj_ptr = vp.getelementptr(main_ptr.get_type().get_deref_type(), main_ptr, int_value(0), int_value(attr_offset), obj_type.get_ptr_type());
        vp.store(rfh, obj_ptr);
    } else {
        // othervise just store
        rfh = conform(rfh, target->get_type().get_deref_type(), env);
        vp.store(rfh, *target);
    }

	return rfh;
}

operand cond_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "cond" << endl;

	ValuePrinter vp(*env->cur_stream);
	string true_label = env->new_label("true.", true);
	string false_label = env->new_label("false.", true);
	string end_label = env->new_label("end.", true);

    // Types of those expression seems to be figured out by semant
    operand result_addr;
    // process types
    op_type then_type(env->get_class()->string_to_type(then_exp->get_type()->get_string()));
    op_type else_type(env->get_class()->string_to_type(else_exp->get_type()->get_string()));

    // find out their least common type, if no such thing exist then error
    op_type least_common_type = find_lesast_common_type(then_type, else_type);
    assert(!least_common_type.is_empty_type());
    result_addr = vp.alloca_mem(least_common_type);

	operand cond = pred->code(env);

	vp.branch_cond(cond, true_label, false_label);

	vp.begin_block(true_label);
	operand then_result = then_exp->code(env);
	vp.store(then_result, result_addr);
	vp.branch_uncond(end_label);

	vp.begin_block(false_label);
	operand else_result = else_exp->code(env);
	vp.store(else_result, result_addr);
	vp.branch_uncond(end_label);

	vp.begin_block(end_label);
    // load result value of the cond
	operand condition_result = vp.load(result_addr.get_type().get_deref_type(), result_addr);

    // bitcast to least common type if necessary
    if(!condition_result.get_type().is_same_with(least_common_type)){
        condition_result = vp.bitcast(condition_result, least_common_type);
    }

	return condition_result;
}

operand loop_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "loop" << endl;
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
	ValuePrinter vp(*env->cur_stream);

	string loop_label = env->new_label("loop.", true);
	string true_label = env->new_label("true.", true);
	string false_label = env->new_label("false.", true);

	// unconditional branch to loop as a basic block needs a terminator
	vp.branch_uncond(loop_label);

    // begining of the loop
	vp.begin_block(loop_label);
	operand cond = pred->code(env);
	vp.branch_cond(cond, true_label, false_label);

	vp.begin_block(true_label);
	body->code(env);
	vp.branch_uncond(loop_label);

	vp.begin_block(false_label);

	return int_value(0);
} 

operand block_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "block" << endl;
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
	int length = body->len();
	operand piece;
    // generate code for each piece in the block
	for (int i = body->first(); body->more(i); i = body->next(i)) {
		piece = body->nth(i)->code(env);
	}

	return piece;
}

operand let_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "let" << endl;
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
	ValuePrinter vp(*env->cur_stream);

    op_type idf_type;
    string type_string = type_decl->get_string();
    // process idf types
    if (type_string == "Int" || type_string == "int"){
        idf_type = op_type(INT32);
    }
    else if (type_string == "Bool" || type_string == "bool") {
        idf_type = op_type(INT1);
    }
    else {
        // if not int or bool
        idf_type = op_type(type_string + "*");
    }

	operand idf = vp.alloca_mem(idf_type);
	operand idf_init = init->code(env);

    // if uninitilized then store null or 0 or false for int and bool
    if(init->no_code()) {
        idf_init = null_value(idf_type);
        if (type_string == "Int" || type_string == "int"){
            idf_init = int_value(0);
        }
        if (type_string == "Bool" || type_string == "bool"){
            idf_init = bool_value(false, true);
        }

    } else {
        if (!idf_init.get_type().is_same_with(idf_type)) {
            idf_init = vp.bitcast(idf_init, idf_type);
        }
    }

    vp.store(idf_init, idf);

    // Add the idf into scope
    env->add_local(identifier, idf);
    operand ret = body->code(env);
    // kill the scope when exit as let is local
	env->kill_local();
	return ret;
}

operand plus_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "plus" << endl;
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
    ValuePrinter vp(*env->cur_stream);

	operand e1_code =  e1->code(env);
	operand e2_code =  e2->code(env);
	return vp.add(e1_code, e2_code);
}

operand sub_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "sub" << endl;
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
    ValuePrinter vp(*env->cur_stream);

	operand e1_code =  e1->code(env);
	operand e2_code =  e2->code(env);
    return vp.sub(e1_code, e2_code);
}

operand mul_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "mul" << endl;
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
    ValuePrinter vp(*env->cur_stream);

	operand e1_code =  e1->code(env);
	operand e2_code =  e2->code(env);
    return vp.mul(e1_code, e2_code);
}

operand divide_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "div" << endl;
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
    ValuePrinter vp(*env->cur_stream);

    operand dividend = e1->code(env);
    operand divisor = e2->code(env);

    label ok_label = env->new_ok_label();
    operand cond = vp.icmp(EQ, divisor, int_value(0));
    vp.branch_cond(cond, "abort", ok_label);
    vp.begin_block(ok_label);
    operand ret = vp.div(dividend, divisor);
    return ret;
}

operand neg_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "neg" << endl;
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
    ValuePrinter vp(*env->cur_stream);

    operand e1_code = e1->code(env);
    return vp.sub(int_value(0), e1_code);
}

operand lt_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "lt" << endl;
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
    ValuePrinter vp(*env->cur_stream);

	operand e1_code =  e1->code(env);
	operand e2_code =  e2->code(env);

	return vp.icmp(LT, e1_code, e2_code);
}

operand eq_class::code(CgenEnvironment *env) {
	if (cgen_debug) std::cerr << "eq" << endl;
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
	ValuePrinter vp(*env->cur_stream);
	operand e1_code = e1->code(env);
	operand e2_code = e2->code(env);

    // if e1 and e1 are not basic types then compare their ptr instead
    if (!((e1->get_type() == Int && e2->get_type() == Int) || (e1->get_type() == Bool && e2->get_type() == Bool)
            || (e1->get_type() == String && e2->get_type() == String))){

        op_type addr_ptr(INT8_PTR);
        e1_code = vp.bitcast(e1_code, addr_ptr);
        e2_code = vp.bitcast(e2_code, addr_ptr);
    }

    return vp.icmp(EQ, e1_code, e2_code);


}

operand leq_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "leq" << endl;
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
    ValuePrinter vp(*env->cur_stream);
	operand e1_code = e1->code(env);
	operand e2_code = e2->code(env);

	return vp.icmp(LE, e1_code, e2_code);
}

operand comp_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "complement" << endl;
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
	ValuePrinter vp(*env->cur_stream);

	operand e1_code = e1->code(env);

	return vp.xor_in(e1_code, bool_value(true, true));
}

operand int_const_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "Integer Constant" << endl;
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL

	return const_value(INT32, token->get_string(), true);
}

operand bool_const_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "Boolean Constant" << endl;
	return bool_value((bool)val, true);
}

operand object_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "Object" << endl;


    ValuePrinter vp(*(env->cur_stream));

    operand load_result;
    string object_name = name->get_string();
    // look up object in env
    operand *object_record = env->lookup(name);

    // the object is not in env then load it from current class's attrs
    if(object_record ==  NULL){
        // look up current class and attr's offset
        object_record = env->lookup(self);
        operand record_ptr = vp.load(object_record->get_type().get_deref_type(), *object_record);
        int attr_offset = env->get_class()->attr_loaction[name->get_string()];

        // -1 as vtables pointer is not in it
        auto obj_type = env->get_class()->attr_types[attr_offset - 1];
        operand obj_ptr = vp.getelementptr(record_ptr.get_type().get_deref_type(), record_ptr, int_value(0), int_value(attr_offset), obj_type.get_ptr_type());
        load_result = vp.load(obj_ptr.get_type().get_deref_type(), obj_ptr);

    } else {
        // if it is in env then just load it from ptr
        load_result = vp.load(object_record->get_type().get_deref_type(), *object_record);
    }
    return load_result;
}

operand no_expr_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "No_expr" << endl;
	return operand();
}

//*****************************************************************
// The next few functions are for node types not supported in Phase 1
// but these functions must be defined because they are declared as
// methods via the Expression_SHARED_EXTRAS hack.
//*****************************************************************

operand static_dispatch_class::code(CgenEnvironment *env) 
{ 
	if (cgen_debug) std::cerr << "static dispatch" << endl;
#ifndef MP3
	assert(0 && "Unsupported case for phase 1");
#else
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
	ValuePrinter vp(*(env->cur_stream));

    // eval expr and if it is int or bool then box it
	operand expr_obj = expr->code(env);
    if(expr_obj.get_type().is_same_with(op_type(INT32))){
        expr_obj = conform(expr_obj, op_type("Int*"), env);
    }
    if(expr_obj.get_type().is_same_with(op_type(INT1))){
        expr_obj = conform(expr_obj, op_type("Bool*"), env);
    }

    // get the static class and dynamic class
    auto static_class =  name_class_map["%" + string(type_name->get_string()) + "*"];
    auto expr_class =  name_class_map[string(expr_obj.get_typename())];

    // if expr is null then abort
    null_value null_val(expr_obj.get_type());
	operand icmp_result = vp.icmp(EQ, expr_obj, null_val);
	string ok_lable = env->new_ok_label();
	vp.branch_cond(icmp_result, "abort", ok_lable);

	vp.begin_block(ok_lable);

    // get function name, offset and type from static class
	string function_name = name->get_string();
	int offset = static_class->function_loaction[function_name];
	op_func_type functor_type = static_class->get_method_type(function_name);


    // load function out from static type vtable
	string vtable_ptr_str = static_class->get_type_name() + "_vtable*";
	op_type vtable_ptr_type(vtable_ptr_str);
	global_value static_type_vt_proto(vtable_ptr_type, static_class->get_type_name()+ "_vtable_prototype");
	operand functor = vp.getelementptr(vtable_ptr_type.get_deref_type(), static_type_vt_proto, int_value(0), int_value(offset), functor_type.get_ptr_type());
	operand loaded_functor = vp.load(functor_type, functor);

    // process args and types
    // bit cast expr_obj if necessary as it is the self arg
	op_type static_type(type_name->get_string());
	static_type = static_type.get_ptr_type();
	if (!expr_obj.get_type().is_same_with(static_type)){
		expr_obj = vp.bitcast(expr_obj, static_type);
	}
	vector<operand> args;
	vector<op_type> arg_types;
	args.push_back(expr_obj);
	arg_types.push_back(expr_obj.get_type());
    auto function_required_types = functor_type.get_args_type();
	for(int i = actual->first(); actual->more(i); i = actual->next(i)){
		Expression formali = actual->nth(i);
		operand arg = formali->code(env);

        // conform if necessary
        arg = conform(arg, function_required_types[i + 1], env);
        args.push_back(arg);
		arg_types.push_back(arg.get_type());
	}

	op_type return_type = functor_type.get_res_type();
	operand result = vp.call(arg_types, return_type, loaded_functor.get_pure_name(), false, args, false);

    // bitcast result, to deal with self type
    auto expr_functor_type = expr_class->get_method_type(function_name);
    auto expr_functor_return_type = expr_functor_type.get_res_type();
    if(!return_type.is_same_with(expr_functor_return_type)){
        result = vp.bitcast(result, expr_functor_return_type);
    }
    return result;

#endif
	return operand();
}

operand string_const_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "string_const" << endl;
#ifndef MP3
	assert(0 && "Unsupported case for phase 1");
#else
    // look up the string object's offset and return it
    string const_var_name = string_const_map[token->get_string()];

    return global_value(op_type("String*"), const_var_name);

#endif
	return operand();
}

operand dispatch_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "dispatch" << endl;
#ifndef MP3
	assert(0 && "Unsupported case for phase 1");
#else
	// ADD CODE HERE AND REPLACE "return operand()" WITH SOMETHING 
	// MORE MEANINGFUL
    ValuePrinter vp(*(env->cur_stream));

    // get expr_obj if it is int32 or int1 then box it
    operand expr_obj = expr->code(env);
    if(expr_obj.get_type().is_same_with(op_type(INT32))){
        expr_obj = conform(expr_obj, op_type("Int*"), env);
    }
    if(expr_obj.get_type().is_same_with(op_type(INT1))){
        expr_obj = conform(expr_obj, op_type("Bool*"), env);
    }

    // get the cgenNode of expr_obj
    auto expr_obj_class =  name_class_map[expr_obj.get_typename()];

    // if expr_obj is null then abort
    null_value null_val(expr_obj.get_type());
    operand icmp_result = vp.icmp(EQ, expr_obj, null_val);
    string okLable = env->new_ok_label();
    vp.branch_cond(icmp_result, "abort", okLable);

    vp.begin_block(okLable);

    // get function name and offset
    string function_name = name->get_string();
    int offset = expr_obj_class->function_loaction[function_name];
    op_func_type functor_type = expr_obj_class->get_method_type(function_name);

    // prepare args and types, the first arg is always self
    vector<operand> args;
    vector<op_type> arg_types;
    args.push_back(expr_obj);
    arg_types.push_back(expr_obj.get_type());
    auto function_required_types = functor_type.get_args_type();
    for(int i = actual->first(); actual->more(i); i = actual->next(i)){
        Expression formali = actual->nth(i);
        operand arg = formali->code(env);

        // conform each arg
        arg = conform(arg, function_required_types[i + 1], env);
        args.push_back(arg);
        arg_types.push_back(arg.get_type());
    }

    // load function from expr_obj's vtable and call it
    string vtable_ptr_str = expr_obj_class->get_type_name() + "_vtable**";
    op_type vtable_ptr_type(vtable_ptr_str);

    operand vtable_ptr = vp.getelementptr(expr_obj.get_type().get_deref_type(), expr_obj, int_value(0), int_value(0), vtable_ptr_type);
    operand vtable = vp.load(vtable_ptr.get_type().get_deref_type(), vtable_ptr);

    operand functor = vp.getelementptr(vtable.get_type().get_deref_type(), vtable, int_value(0), int_value(offset), functor_type.get_ptr_type());
    operand loaded_functor = vp.load(functor_type, functor);

    op_type return_type = functor_type.get_res_type();
    operand result = vp.call(arg_types, return_type, loaded_functor.get_pure_name(), false, args, false);

    return result;

#endif
	return operand();
}

operand typcase_class::code(CgenEnvironment *env)
{
    if (cgen_debug)
        std::cerr << "typecase::code()" << endl;
#ifndef MP3
    assert(0 && "Unsupported case for phase 1");
#else
    ValuePrinter vp(*env->cur_stream);
    CgenClassTable *ct = env->get_class()->get_classtable();

    string header_label = env->new_label("case.hdr.", false);
    string exit_label = env->new_label("case.exit.", false);

    // Generate code for expression to select on, and get its static type
    operand code_val = expr->code(env);
    operand expr_val = code_val;
    string code_val_t = code_val.get_typename();
    op_type join_type = env->type_to_class(type)->get_type_name();
    CgenNode *cls = env->type_to_class(expr->get_type());

    // Check for case on void, which gives a runtime error
    if (code_val.get_type().get_id() != INT32_PTR && code_val.get_type().get_id() != INT1_PTR) {
        op_type bool_type(INT1), empty_type;
        null_value null_op(code_val.get_type());
        operand icmp_result(bool_type, env->new_name());
        vp.icmp(*env->cur_stream, EQ, code_val, null_op, icmp_result);
        string ok_label = env->new_ok_label();
        vp.branch_cond(icmp_result, "abort", ok_label);
        vp.begin_block(ok_label);
    }

    operand tag = get_class_tag(expr_val, cls, env);
    vp.branch_uncond(header_label);
    string prev_label = header_label;
    vp.begin_block(header_label);

    // Get result type of case expression
    branch_class *b = (branch_class *)cases->nth(cases->first());
    string case_result_type = b->get_expr()->get_type()->get_string();
    if (case_result_type == "SELF_TYPE")
        case_result_type = env->get_class()->get_type_name();

    // Allocate space for result of case expression
    op_type alloca_type(case_result_type, 1);
    operand alloca_final(alloca_type, env->new_name());
    env->branch_operand = alloca_final;
    vp.alloca_mem(*env->cur_stream, alloca_type, alloca_final);

    std::vector<operand> values;
    env->next_label = exit_label;

    // Generate code for the branches
    for (int i=ct->get_num_classes()-1; i >= 0; --i) {
        for (int j=cases->first(); cases->more(j); j = cases->next(j)) {
            if (i == ct->lookup(cases->nth(j)->get_type_decl())->get_tag()) {
                string prefix = string("case.") + itos(i) + ".";
                string case_label = env->new_label(prefix, false);
                vp.branch_uncond(case_label);
                vp.begin_block(case_label);
                operand val = cases->nth(j)->code(expr_val, tag,
                                                  join_type, env);
                values.push_back(val);
            }
        }
    }

    // Abort if there was not a branch covering the actual type
    vp.branch_uncond("abort");

    // Done with case expression: get final result
    env->new_label("", true);
    vp.begin_block(exit_label);
    operand final_result(alloca_type, env->new_name());
    alloca_final.set_type(alloca_final.get_type().get_ptr_type());
    vp.load(*env->cur_stream, alloca_final.get_type().get_deref_type(), alloca_final,
            final_result);
    alloca_final.set_type(alloca_final.get_type().get_deref_type());

    if (cgen_debug)
        cerr << "Done typcase::code()" << endl;
    return final_result;
#endif
}

operand new__class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "newClass" << endl;
#ifndef MP3
	assert(0 && "Unsupported case for phase 1");
#else
	ValuePrinter vp(*(env->cur_stream));

    // name is always class name to _new
	string function_name = string(type_name->get_string()) + "_new";
	op_type result_type(string(type_name->get_string()) + "*");

	// call the global new function
	operand call_result = vp.call(result_type, function_name, true);
	return call_result;

#endif
	return operand();
}

operand isvoid_class::code(CgenEnvironment *env) 
{
	if (cgen_debug) std::cerr << "isvoid" << endl;
#ifndef MP3
	assert(0 && "Unsupported case for phase 1");
#else
    ValuePrinter vp(*(env->cur_stream));
    operand expr_result = e1->code(env);

    // check whether equal to null type
    operand is_void_result = vp.icmp(EQ, expr_result, operand(null_value(expr_result.get_type())));
    return is_void_result;

#endif
	return operand();
}

// Create the LLVM Function corresponding to this method.
void method_class::layout_feature(CgenNode *cls) 
{
#ifndef MP3
	assert(0 && "Unsupported case for phase 1");
#else
    vector<op_type> formal_list;
    // the first arg is always self
    string className = cls->get_type_name();
    formal_list.push_back(cls->string_to_type(className));

    // process funcrion arg types
    for (int x = formals->first(); formals->more(x); x = formals->next(x)) {
        Formal formali = formals->nth(x);
        op_type temp_type = cls->string_to_type(formali->get_type_decl()->get_string());
        formal_list.push_back(temp_type);
    }

    op_type vt_return_type(cls->string_to_type(return_type->get_string()));

    // process type info and push them to temp vtable
    op_func_type vt_function_type(vt_return_type, formal_list);
    cls->temp_vtable_types.push_back(vt_function_type);
    cls->temp_vtable_return_types.push_back(return_type->get_string());
    cls->temp_vtable_args.push_back(formal_list);
    cls->add_function_loaction(name->get_string());
    cls->function_names.push_back(name->get_string());
    cls->function_rets[name->get_string()] = vt_return_type.get_name();
    cls->add_func_type(vt_function_type);

    //add to temp vtable prototype
    string function_name = cls->get_type_name() + "_" + name->get_string(); // concat func name to type name
    op_type function_type(function_name);
    global_value function_name_value(function_type, function_name);
    const_value function_entry(function_type, function_name_value.get_name(), false);
    cls->temp_vtable_protos.push_back(function_entry);


#endif
}

// If the source tag is >= the branch tag and <= (max child of the branch class) tag,
// then the branch is a superclass of the source
operand branch_class::code(operand expr_val, operand tag,
				op_type join_type, CgenEnvironment *env) {
    cerr<< "branch class" << endl;
#ifndef MP3
    assert(0 && "Unsupported case for phase 1");
#else
    operand empty;
    ValuePrinter vp(* env->cur_stream);
    if  (cgen_debug)
        cerr << "In branch_class::code()" << endl;

    CgenNode *cls = env->get_class()->get_classtable()->lookup(type_decl);
    int my_tag = cls->get_tag();
    int max_child = cls->get_max_child();

    // Generate unique labels for branching into >= branch tag and <= max child
    string sg_label =
            env->new_label(string("src_gte_br") + "." + itos(my_tag) + ".", false);
    string sl_label =
            env->new_label(string("src_lte_mc") + "." + itos(my_tag) + ".", false);
    string exit_label =
            env->new_label(string("br_exit") + "." + itos(my_tag) + ".", false);

    int_value my_tag_val(my_tag);
    op_type old_tag_t(tag.get_type()), i32_t(INT32);
    tag.set_type(i32_t);

    // Compare the source tag to the class tag
    operand icmp_result = vp.icmp(LT, tag, my_tag_val);
    vp.branch_cond(icmp_result, exit_label, sg_label);
    vp.begin_block(sg_label);
    int_value max_child_val(max_child);

    // Compare the source tag to max child
    operand icmp2_result = vp.icmp(GT, tag, max_child_val);
    vp.branch_cond(icmp2_result, exit_label, sl_label);
    vp.begin_block(sl_label);
    tag.set_type(old_tag_t);

    // Handle casts of *arbitrary* types to Int or Bool.  We need to:
    // (a) cast expr_val to boxed type (struct Int* or struct Bool*)
    // (b) unwrap value field from the boxed type
    // At run-time, if source object is not Int or Bool, this will never
    // be invoked (assuming no bugs in the type checker).
    if (cls->get_type_name() == "Int" || cls->get_type_name() == "Bool") {
        op_type lbl_t(cls->get_type_name(), 1);
        expr_val = conform(expr_val, lbl_t, env);
    }

    // If the case expression is of the right type, make a new local
    // variable for the type-casted version of it, which can be used
    // within the expression to evaluate on this branch.
    op_type alloc_type(cls->get_type_name(), 1);
    operand alloc_op = vp.alloca_mem(alloc_type);
    operand conf_result = conform(expr_val, alloc_type,  env);
    vp.store(conf_result, alloc_op);
    env->add_local(name, alloc_op);

    // Generate code for the expression to evaluate on this branch
    operand val = conform(expr->code(env), join_type.get_ptr_type(), env);
    operand conformed = conform(val, env->branch_operand.get_type(), env);
    env->branch_operand.set_type(env->branch_operand.get_type()
                                         .get_ptr_type());
    // Store result of the expression evaluated
    vp.store(conformed, env->branch_operand);
    env->branch_operand.set_type(env->branch_operand.get_type()
                                         .get_deref_type());
    env->kill_local();
    // Branch to case statement exit label
    vp.branch_uncond(env->next_label);
    vp.begin_block(exit_label);
    if (cgen_debug)
        cerr << "Done branch_class::code()" << endl;
    return conformed;
#endif
}

// Assign this attribute a slot in the class structure
void attr_class::layout_feature(CgenNode *cls)
{
#ifndef MP3
	assert(0 && "Unsupported case for phase 1");
#else
	// process attr types and store them
	string type = string(type_decl->get_string());
    // don't deal with self type as specified in description
	if(type != string("SELF_TYPE")){
		if (type == "Int" || type == "int"){
            cls->attr_types.emplace_back(INT32);
		}
		else if (type == "Bool" || type == "bool") {
			cls->attr_types.emplace_back(INT1);
		}
		else if (type == "sbyte*"){
			cls->attr_types.emplace_back(INT8_PTR);
		}
		else {
			cls->attr_types.emplace_back(type + "*");
		}

		// store those names for future indexing
		cls->add_attr_loaction(name->get_string());
		cls->attr_names.emplace_back(name->get_string());
	}

#endif
}

void attr_class::code(CgenEnvironment *env)
{
#ifndef MP3
	assert(0 && "Unsupported case for phase 1");
#else
	// ADD CODE HERE
	if(env->generating_methods){
		return;
	}

	ValuePrinter vp(*(env->cur_stream));
	if (cgen_debug) std::cerr << "attribute!\n";

	//evaluate the expression
	operand init_result = init->code(env);

	int_value zero_int_op(0);
	int_value attr_index(env->get_class()->attr_loaction[name->get_string()]);

    string attr_name = name->get_string();
    op_type attr_type = env->get_class()->get_attr_type(attr_name);
    op_type expr_op_type;
    if (init->no_code()){
        // if no init, then store null or 0 or false for int and bool type
        expr_op_type = attr_type.get_ptr_type();
        init_result = null_value(attr_type);
        if (attr_type.is_same_with(op_type(INT32))){
            init_result = int_value(0);
        }
        if (attr_type.is_same_with(op_type(INT1))){
            init_result = bool_value(false, true);
        }
    } else {
        init_result = conform(init_result, attr_type, env);
        expr_op_type = init_result.get_type().get_ptr_type();
    }

    //	%tmp.24 = getelementptr %Main* %tmp.21, i32 0, i32 1
    operand * attr_ret_op = new operand( vp.getelementptr(
			env->malloc_result.get_type().get_deref_type(),
			env->malloc_result,
			zero_int_op,
			attr_index,
			expr_op_type));

    //	store i32 5, i32* %tmp.24
    vp.store(init_result, * attr_ret_op);

#endif
}

// process string to types, used to deal with return type string
op_type CgenNode::string_to_type(string typeString) {
    string classType = get_type_name();
    if (typeString == "SELF_TYPE") {
        return op_type(classType, 1);
    }
    else if (classType != "Bool" && (typeString == "Bool" || typeString == "bool")){
        return op_type(INT1);
    }
    else if (classType != "Int" && (typeString == "Int" || typeString == "int")){
        return op_type(INT32);
    }
    return op_type(typeString + "*");
}


void CgenNode::inherit() {

    CgenNode * parent = get_parentnd();

    string parent_type = parent->get_type_name();
    string self_type = get_type_name();

    //add inherited functions to vtable

	//Check inherited method
    for (size_t i = 0; i < parent->temp_vtable_types.size(); i++) {
        string function_name = parent->function_names[i];

        //If there is a function override this function from parent then skip it
        if(check_override(function_name)){
            continue;
        }

        // get parent's method's return type and process it
        op_type vt_ft_func = string_to_type(parent->temp_vtable_return_types.at(i));

        // record info related to args
        vector<op_type> arg_types;
        //change the first arg type to child class type
        op_type selfPtr(self_type + "*");
        arg_types.push_back(selfPtr);
        auto parent_function_arg = parent->temp_vtable_args[i];
        for (int j = 1; j < parent_function_arg.size(); j++) {
            arg_types.push_back(parent_function_arg[j]);
        }
        temp_vtable_args.push_back(parent->temp_vtable_args[i]);
        temp_vtable_return_types.push_back(parent->temp_vtable_return_types.at(i));

        // record info related to the inherited method's type
        op_func_type new_function_type(vt_ft_func, arg_types);
        temp_vtable_types.push_back(new_function_type);
        add_function_loaction(parent->function_names[i]);
        function_rets[parent->function_names[i]] = vt_ft_func.get_name();
        add_func_type(new_function_type);

        // process ret type and bitcast
        string parent_ret_type = parent->string_to_type(parent->temp_vtable_return_types.at(i)).get_name();
        auto parent_function_info = parent->temp_vtable_types[i];
        string parent_function_name = parent->temp_vtable_protos[i].get_value();
        //push things into to temp vtable_proto
        string inherited_string = "bitcast (" + parent_function_info.get_name() + " "+ parent_function_name + " to " + new_function_type.get_name() + " )";
        const_value inherited_const(INT32, inherited_string, false);
        temp_vtable_protos.push_back(inherited_const);
        function_names.push_back(function_name);
    }

	//inherit attributes as it is not legal to override a attribute, I won't check it for now
	auto parent_attrs = parent->attr_types;
	auto parent_attr_names = parent->attr_names;
	for (int k = 0; k < parent_attrs.size(); ++k) {
		attr_types.push_back(parent_attrs[k]);

		// store those names for furture indexing
		attr_names.push_back(parent_attr_names[k]);
		add_attr_loaction(parent_attr_names[k]);

	}
}

// check whether a function is overrided
bool CgenNode::check_override(string name){
	//loop through all the name to check whether there is already a same one
    for (size_t i = 0; i < function_names.size(); i++){
        if (function_names[i] == name){
            return true;
        }
    }
    return  false;
}

// a function to find the least common type for two type, used for cond
op_type find_lesast_common_type(op_type a, op_type b){
    if(a.is_same_with(b)){
        return a;
    }

    string a_name = a.get_name();
    string b_name = b.get_name();

    // look up their corresponding cgenNode
    auto b_class_node = name_class_map[b_name];
    auto a_class_node = name_class_map[a_name];

    // Go through all a's ancestors and add them to a vector
    vector <CgenNode *> a_ancestors;
    auto a_parent = a_class_node->get_parentnd();
    while(a_parent->get_parent() != No_class){
        a_ancestors.push_back(a_parent);
        a_parent = a_parent->get_parentnd();
    }

    // Go through all b's ancestors and add them to a vector
    vector <CgenNode *> b_ancestors;
    auto b_parent = b_class_node->get_parentnd();
    while(b_parent->get_parent() != No_class){
        b_ancestors.push_back(b_parent);
        b_parent = b_parent->get_parentnd();
    }

    // loop through their ancestors and look for first cgenNode that is shared by both of them
    for (auto i : a_ancestors){
        for (auto j : b_ancestors){
            if (i == j){
                // return its type
                string type_name = string(i->get_type_name()) + "*";
                return op_type(type_name);
            }
        }
    }

    // if nothing found then return empty
    return op_type(EMPTY);
}