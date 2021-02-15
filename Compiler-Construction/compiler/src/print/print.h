
/**
 * @file print.h
 *
 * Functions to print node structures
 * 
 */

#ifndef _SAC_PRT_NODE_H_
#define _SAC_PRT_NODE_H_

#include "types.h"

extern node *PRTstmts (node * arg_node, info * arg_info);
extern node *PRTassign (node * arg_node, info * arg_info);
extern node *PRTvar (node * arg_node, info * arg_info);
extern node *PRTvarlet (node * arg_node, info * arg_info);
extern node *PRTbinop (node * arg_node, info * arg_info);
extern node *PRTfloat (node * arg_node, info * arg_info);
extern node *PRTnum (node * arg_node, info * arg_info);
extern node *PRTbool (node * arg_node, info * arg_info);
extern node *PRTsymboltableentry (node * arg_node, info * arg_info);
extern node *PRTerror (node * arg_node, info * arg_info);

extern node *PRTdoPrint( node *syntaxtree);

extern node *PRTprogram (node * arg_node, info * arg_info);
extern node *PRTdecls (node * arg_node, info * arg_info);
extern node *PRTfundec (node * arg_node, info * arg_info);
extern node *PRTfundef (node * arg_node, info * arg_info);
extern node *PRTfunheader (node * arg_node, info * arg_info);
extern node *PRTrettype (node * arg_node, info * arg_info);
extern node *PRTglobaldec (node * arg_node, info * arg_info);
extern node *PRTglobaldef (node * arg_node, info * arg_info);
extern node *PRTparam (node * arg_node, info * arg_info);
extern node *PRTfunbody (node * arg_node, info * arg_info);
extern node *PRTvardecl (node * arg_node, info * arg_info);
extern node *PRTifelse (node * arg_node, info * arg_info);
extern node *PRTwhile (node * arg_node, info * arg_info);
extern node *PRTdowhile (node * arg_node, info * arg_info);
extern node *PRTfor (node * arg_node, info * arg_info);
extern node *PRTmonop (node * arg_node, info * arg_info);


#endif /* _SAC_PRT_NODE_H_ */
