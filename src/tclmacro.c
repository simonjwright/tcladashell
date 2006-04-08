/*
 * tclmacro.c --
 *
 *    This file encapsulates calls to all tcl.h macro functions into
 *    C function calls.  These can then be called from Ada.  This
 *    avoids having to translate the macro.
 */

#include <tcl.h>
#include <malloc.h>

int Tcl_GetObjId (objPtr)
   register struct Tcl_Obj *objPtr;
{
   return ((int) objPtr);
}

char *Tcl_GetObjTypeName (objPtr)
   register struct Tcl_Obj *objPtr;
{
   struct Tcl_ObjType *typePtr;
   if (objPtr == NULL) {
      return NULL;
   } else {
      typePtr = objPtr->typePtr;
      if (typePtr == NULL) {
	 return NULL;
      } else {
	 return (typePtr->name);
      }
   }
}

int Tcl_GetRefCount (objPtr)
   register struct Tcl_Obj *objPtr;
{
   if (objPtr == NULL) {
      return 0;
   } else {
      return objPtr->refCount;
   }
}

void Tcl_PrintObj (objPtr)
   register struct Tcl_Obj *objPtr;
{
   int len;
   if (objPtr == NULL) {
      printf ("NULL");
   } else {
      printf ("s=\"%s\" ", Tcl_GetStringFromObj (objPtr, &len));
      printf ("t=%s ",     Tcl_GetObjTypeName (objPtr));
      printf ("c=%d",      Tcl_GetRefCount (objPtr));
   }
}

int Tcl_CallIncrRefCount (objPtr)
   register struct Tcl_Obj *objPtr;
{
   return Tcl_IncrRefCount (objPtr);
}

void Tcl_CallDecrRefCount (objPtr)
   register struct Tcl_Obj *objPtr;
{
   Tcl_DecrRefCount (objPtr);
}

int Tcl_CallIsShared (objPtr)
   register struct Tcl_Obj *objPtr;
{
   return Tcl_IsShared (objPtr);
}

int Tcl_CallDStringLength (dsPtr)
   register Tcl_DString *dsPtr;
{
   return Tcl_DStringLength (dsPtr);
}

char *Tcl_CallDStringValue (dsPtr)
   register Tcl_DString *dsPtr;
{
   return Tcl_DStringValue (dsPtr);
}

/*
void Tcl_CallFreeResult (interp)
   register Tcl_Interp *interp;
{
   Tcl_FreeResult (interp);
}
*/

ClientData Tcl_CallGetHashValue (h)
   register Tcl_HashEntry *h;
{
   return Tcl_GetHashValue (h);
}

void Tcl_CallSetHashValue (h, value)
   register Tcl_HashEntry *h;
   register ClientData value;
{
   Tcl_SetHashValue (h, value);
}

char *Tcl_CallGetHashKey (tablePtr, h)
   register Tcl_HashTable *tablePtr;
   register Tcl_HashEntry *h;
{
   return Tcl_GetHashKey (tablePtr, h);
}

Tcl_HashEntry *Tcl_CallFindHashEntry (tablePtr, key)
   register Tcl_HashTable *tablePtr;
   register char          *key;
{
   return Tcl_FindHashEntry (tablePtr, key);
}

Tcl_HashEntry *Tcl_CallCreateHashEntry (tablePtr, key, newPtr)
   register Tcl_HashTable *tablePtr;
   register char          *key;
   register int           *newPtr;
{
   return Tcl_CreateHashEntry (tablePtr, key, newPtr);
}



