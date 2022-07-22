#ifndef CSTX_H
#define CSTX_H

#include <QObject>

typedef struct CSTX
{
    /***  HEADER  ***/
        /**  Version = tuple of numbers, {CBI, VSD, VSN}  **/
            /*  CBI = number, >=0 && <256, the Crown-Block Index    */
            int CBI;
            /*  VSD = number, >=0 && <256, version serial domain    */
            int VSD;
            /*  VSN = number, >=0 && <256, version serial number    */
            int VSN;
        /**  Service = tuple of numbers, {SD, SSD}   **/
            /*  SD = number, >=0 && <256, service domain    */
            int SD;
            /*  SSD = number, >=0 && <256, service sub-domain   */
            int SSD;
        /** Procedure = tuple of numbers, {pRange, pPack} **/
            /*  pRange = number, >=0 && <256, procedure range   */
            int pRange;
            /*  pPack = number, >=0 && <256, procedure package  */
            int pPack;
        /**  Process = tuple of numbers, {pMod, pRef} **/
            /*  pMod = number, >=0 && <256, process module  */
            int pMod;
            /*  pRef = number, >=0 && <256, process reference  */
            int pRef;
    /***  Body  ***/
        QByteArray parameters;
    /***  Footer  ***/
        QByteArray authentication;
}
CSTX_t;

#endif // CSTX_H
