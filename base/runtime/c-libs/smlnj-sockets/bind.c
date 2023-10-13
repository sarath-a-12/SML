/* bind.c
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 */

#include "sockets-osdep.h"
#include INCLUDE_SOCKET_H
#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-c.h"
#include "cfun-proto-list.h"

/* _ml_Sock_bind : (sock * addr) -> unit
 */
ml_val_t _ml_Sock_bind (ml_state_t *msp, ml_val_t arg)
{
    int		sock = REC_SELINT(arg, 0);
    ml_val_t	addr = REC_SEL(arg, 1);
    int		sts;

    sts = bind (
	sock,
	GET_SEQ_DATAPTR(struct sockaddr, addr),
	GET_SEQ_LEN(addr));

    CHK_RETURN_UNIT(msp, sts);

} /* end of _ml_Sock_bind */
