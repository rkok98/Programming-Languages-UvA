/*****************************************************************************
 *
 * Module: sum_ints
 *
 * Prefix: SI
 *
 * Description:
 *
 * This module implements a demo traversal of the abstract syntax tree that
 * sums up all integer constants and prints the result at the end of the traversal.
 *
 *****************************************************************************/


#include "count_arthmtc_op.h"

#include "types.h"
#include "tree_basic.h"
#include "traverse.h"
#include "dbug.h"

#include "memory.h"
#include "ctinfo.h"


/*
 * INFO structure
 */

struct INFO {
    int cnt_add;
    int cnt_sub;
    int cnt_mul;
    int cnt_div;
    int cnt_mod;
};


/*
 * INFO macros
 */

#define INFO_CNT_ADD(n)  ((n)->cnt_add)
#define INFO_CNT_SUB(n)  ((n)->cnt_add)
#define INFO_CNT_MUL(n)  ((n)->cnt_mul)
#define INFO_CNT_DIV(n)  ((n)->cnt_div)
#define INFO_CNT_MOD(n)  ((n)->cnt_mod)


/*
 * INFO functions
 */

static info *MakeInfo(void)
{
    info *result;

    DBUG_ENTER( "MakeInfo");

    result = (info *)MEMmalloc(sizeof(info));

    INFO_CNT_ADD( result) = 0;
    INFO_CNT_SUB( result) = 0;
    INFO_CNT_MUL( result) = 0;
    INFO_CNT_DIV( result) = 0;
    INFO_CNT_MOD( result) = 0;

    DBUG_RETURN( result);
}

static info *FreeInfo( info *info)
{
    DBUG_ENTER ("FreeInfo");

    info = MEMfree( info);

    DBUG_RETURN( info);
}


/*
 * Traversal functions
 */

node *CAObinop (node *arg_node, info *arg_info)
{
    DBUG_ENTER("CAObinop");

    if (BINOP_OP( arg_node) == BO_add) {
        INFO_CNT_ADD( arg_info) += 1;
    }

    if (BINOP_OP( arg_node) == BO_sub) {
        INFO_CNT_SUB( arg_info) += 1;
    }

    if (BINOP_OP( arg_node) == BO_mul) {
        INFO_CNT_MUL( arg_info) += 1;
    }

    if (BINOP_OP( arg_node) == BO_div) {
        INFO_CNT_DIV( arg_info) += 1;
    }

    if (BINOP_OP( arg_node) == BO_mod) {
        INFO_CNT_MOD( arg_info) += 1;
    }

    DBUG_RETURN( arg_node);
}


node *CAOmodule(node *arg_node, info *arg_info) {
    DBUG_ENTER("CAOmodule");

    info *info = MakeInfo();

    TRAVdo(MODULE_NEXT(arg_node), info);

    MODULE_ADD(arg_node) = INFO_CNT_ADD(info);

    DBUG_RETURN(arg_node);
}

node *CAOdoCountBinOp( node *syntaxtree)
{
    info *arg_info;

    DBUG_ENTER("CAOdoCountBinOp");

    arg_info = MakeInfo();

    TRAVpush( TR_cao);
    syntaxtree = TRAVdo( syntaxtree, arg_info);
    TRAVpop();



    // node *module = TBmakeModule(INFO_CNT_MUL(arg_info), syntaxtree);

    // CTInote( "Amount of multipliers %d", INFO_CNT_MUL( arg_info));

    arg_info = FreeInfo( arg_info);

    DBUG_RETURN(syntaxtree);
}
