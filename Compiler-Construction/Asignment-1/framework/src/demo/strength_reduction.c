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

node *SRbinop(node *arg_node, info *arg_info) {
    DBUG_ENTER("SRbinop");

    // Check if operator is a multiplier operator
    if (BINOP_OP(arg_node) == BO_mul) {
        // Declare multiplier and var node
        int MULTIPLIER;
        char *VAR;

        if ((NODE_TYPE(BINOP_LEFT(arg_node)) == N_var)
            && (NODE_TYPE(BINOP_RIGHT(arg_node)) == N_num)) {

            MULTIPLIER = NUM_VALUE(BINOP_RIGHT(arg_node));
            VAR = VAR_NAME(BINOP_LEFT(arg_node));
        } else if ((NODE_TYPE(BINOP_LEFT(arg_node)) == N_num)
                   && (NODE_TYPE(BINOP_RIGHT(arg_node)) == N_var)) {

            VAR = VAR_NAME(BINOP_RIGHT(arg_node));
            MULTIPLIER = NUM_VALUE(BINOP_LEFT(arg_node));
        }
        else {
            // Returns if there's no num and var node
            DBUG_RETURN(arg_node);
        }

        if (MULTIPLIER > 2) {
            BINOP_OP(arg_node) = BO_add;
            BINOP_RIGHT(arg_node) = TBmakeVar(STRcpy(VAR));

            BINOP_LEFT(arg_node) = TBmakeBinop(BO_mul, TBmakeVar(STRcpy(VAR)), TBmakeNum(MULTIPLIER - 1));
        } else {
            arg_node = TBmakeBinop(BO_add, TBmakeVar(STRcpy(VAR)), TBmakeVar(STRcpy(VAR)));
        }
    }

    BINOP_LEFT(arg_node) = TRAVdo(BINOP_LEFT(arg_node), arg_info);
    DBUG_RETURN(arg_node);
}

node *SRdoStrengthReduction(node *syntaxtree) {
    DBUG_ENTER("SRdoStrengthReduction");

    TRAVpush(TR_sr);
    syntaxtree = TRAVdo(syntaxtree, NULL);
    TRAVpop();

    DBUG_RETURN(syntaxtree);
}
