#include "count_arthmtc_op.h"

#include "types.h"
#include "tree_basic.h"
#include "lookup_table.h"
#include "traverse.h"
#include "dbug.h"

#include "memory.h"
#include "ctinfo.h"

#include "stdio.h"


struct INFO {
    lut_t *table;
};

#define INFO_LUT(n)  ((n)->table)

static info *MakeInfo(void) {
    info *result;

    DBUG_ENTER("MakeInfo");

    result = (info *) MEMmalloc(sizeof(info));

    INFO_LUT(result) = LUTgenerateLut();

    DBUG_RETURN(result);
}

static info *FreeInfo(info *info) {
    DBUG_ENTER("FreeInfo");

    info = MEMfree(info);

    DBUG_RETURN(info);
}

node *CIDvar(node *arg_node, info *arg_info) {
    DBUG_ENTER("CIDvar");
    /*
    if (LUTsearchInLutS(STRcpy(VAR_NAME(arg_node))) != NULL) {
        printf("AAAA");
    }
     */

    printf("BBBB");
    DBUG_RETURN(arg_node);
}

node *CIDvarlet(node *arg_node, info *arg_info) {
    DBUG_ENTER("CIDvarlet");
    DBUG_RETURN(arg_node);
}

node *CIDdoCountIds(node *syntaxtree) {
    info *arg_info;

    DBUG_ENTER("CIDdoCountIds");

    arg_info = MakeInfo();

    TRAVpush(TR_cid);
    syntaxtree = TRAVdo(syntaxtree, arg_info);
    TRAVpop();

    arg_info = FreeInfo(arg_info);

    DBUG_RETURN(syntaxtree);
}
