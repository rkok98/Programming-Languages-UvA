/*****************************************************************************
 *
 * Module: strength_reduction
 *
 * Prefix: SR
 *
 * Description:
 *
 * This module implements a demo traversal of the abstract syntax tree that
 * sums up all integer constants and prints the result at the end of the traversal.
 *
 *****************************************************************************/


#include "strength_reduction.h"

#include "types.h"
#include "tree_basic.h"
#include "traverse.h"
#include "dbug.h"

#include "memory.h"
#include "free.h"
#include "str.h"

/*
 * Traversal functions
 */

node *SRbinop(node *arg_node, info *arg_info) {
    DBUG_ENTER("SRbinop");

    /*
     * Extremely important:
     *  we must continue to traverse the abstract syntax tree !!
     */
    BINOP_RIGHT(arg_node) = TRAVdo(BINOP_RIGHT(arg_node), arg_info);
    BINOP_LEFT(arg_node) = TRAVdo(BINOP_LEFT(arg_node), arg_info);

    // 1. Check if operator is a multiplier operator
    if (BINOP_OP(arg_node) == BO_mul) {
        // 2. Declare var node and multiplier node
        int MULTIPLIER;
        node * VAR_NODE;

        // Writes var and multiplier to the right node
        // Returns if there's no num and var node
        if ((NODE_TYPE(BINOP_LEFT(arg_node)) == N_var)
            && (NODE_TYPE(BINOP_RIGHT(arg_node)) == N_num)) {
            MULTIPLIER = NUM_VALUE(BINOP_RIGHT(arg_node)) - 1;
            VAR_NODE = BINOP_LEFT(arg_node);
        } else if ((NODE_TYPE(BINOP_LEFT(arg_node)) == N_num)
                   && (NODE_TYPE(BINOP_RIGHT(arg_node)) == N_var)) {
            MULTIPLIER = NUM_VALUE(BINOP_LEFT(arg_node)) - 1;
            VAR_NODE = BINOP_RIGHT(arg_node);
        } else {
            DBUG_RETURN(arg_node);
        }

        // 1. Make Parent addition
        BINOP_OP(arg_node) = BO_add;

        // 2. Make right node the var
        BINOP_RIGHT(arg_node) = VAR_NODE;

        // 3. Make left node var * multiplier
        BINOP_LEFT(arg_node) = TBmakeBinop(BO_mul, TBmakeNum(2), TBmakeNum(MULTIPLIER));
    }

    DBUG_RETURN(arg_node);
}


/*
 * Traversal start function
 */

node *SRdoStrengthReduction(node *syntaxtree) {
    DBUG_ENTER("SRdoStrengthReduction");

    TRAVpush(TR_sr);
    syntaxtree = TRAVdo(syntaxtree, NULL);
    TRAVpop();

    DBUG_RETURN(syntaxtree);
}
