# Abstract
A distributed platform to compose and compute information services, from a simple mathematical operation to rotation of a solar cell on a spacecraft.<br/>
The distributed storage records worthful information for centuries as D20-Blockchain with the consensus mechanism of PoS.<br/>
The decentralized network on top of UDP/IP routed by distributed hash table provides peer-to-peer connections, which enable nodes to stablish end-to-end secure transmission tunnel, even though they use dynamic IP addresses behind ISPs, thanks to Hole-Punching mechanism.<br/>
The cross-platform infrastructure enhances developers to implement and deploy their services coded with C/C++, Erlang/OTP, JAVA, Python, JavaScript and other supported programming languages and frameworks on multiple platforms or software environments. It provides a laboratory of distributed systems for developers to design, implement, deploy and test services for example on a local IoT system. Furthermore, web application providers are able to serve their services using the advantages of distributed computing to reduce serving costs.<br/>
Free Flow of Information and Privacy are the CIS duty. Everybody can provide, access and use information services on CIS anonymously.<br/>


---------

## Cessor Information Systems, CIS

A distributed service-oriented and TX-based platform to perform particular processor of a cessor service, CS.<br />
CS is a distributed TX-based software-service documented and coded by supported programming languages.<br />
CIS provides cessor service transactions, CSTX as the I/O medium to communicate with other CS on CIS.<br />
CSTX is the structured service information object integrated with CIS.<br />
CS composes and computes CSTX using cessor service transaction protocol, CSTP.<br />
CSTP addressed CSTX to perform particular processor of a CS on CIS.<br />
CSTP maps a set of functions in a module of a package in a specific range on an allocated or local CS domain.<br />

---------

## Cessor Service Transaction Protocol, CSTP

CIS provides service model specifying how a procedure has to be called.
CIS model has 4 layers to address a particular process of a procedure on CIS.

1. Version Layer<br />
	- Protocol version control = Crown-Block index
	- Version serial domain
	- Version serial number
2. Service Layer
	- Service domain
	- Service sub-domain
3. Procedure Layer
	- Procedure range
	- Procedure package
4. Process Layer
	- Process module
	- Processor reference

![Alt text][cstp]

---------

# Cessor Service Transaction, CSTX
- CSTX is the structured service information object and the I/O medium on CIS.
- CSTX contains three parts:<br/>

1. TX header<br/>
    - The header addresses to a unique processor on CIS.<br/>
    - TX header contains four parts:<br/>

	I. Protocol Version Control
    ```sh
        Protocol Version Control, PVC       {CBI,VSD,VSN}       3 Octets

    CBI = First octet is allocated for Crown-Block index, on which the protocol is issued.
    VSD = Second octet is allocated for the version serial domain.
    VSN = Third octet is allocated for the version serial number.
    *VSN <255> is reserved for version extension.
    ```

	II. Service Domain Range

    ```sh
        Service Domain Range, SDR           {SDR0,SDR1}         2 Octets

    SDR0 = First octet is allocated for service domain.
    SDR1 = Second octet is allocated for service sub-domain.

    *Service domain <0> has following reserved sub-domains
                        <0> .. <3>	system services

    *Service domain 255 is reserved for local service sub-domains
    ```

	III. Procedure Source
    ```sh
        Procedure Source, PS                {PS0,PS1}           2 Octets

    PS0 = First octet is allocated for service procedure range.
	PS1 = Second octet is allocated for service procedure package.
    ```

	IV. Process Reference

    ```sh
        Process Reference, PR               {PR0,PR1}           2 Octets

    PR0 = First octet is allocated for service process module.
    PR1 = Second octet is allocated for service process function/s, Processor.
    *Processor <255> is reserved for processor extension.
    ```


2. TX body<br/>
    The data block of transaction represents associated parameters of corresponding processor as function arguments.
	
3. TX footer<br/>
	According to process reference, CSTX may need authentication information to be validated.
	Based on the process reference, the information can be a Signature or a MAC.<br/>
    I. TX authentication information
    ```sh
        Authentication information              {...}           K Octets

    K = TX authentication length
    ```
    II. TX authentication length
    ```sh
        Authentication length                   {K}             1 Octet

    256 > K >= 0, The length of authentication information
    ```
    III. TX authentication version
    ```sh
        Authentication version                  {Ver}           1 Octet

    256 > Ver >= 0, The version of authentication information on CIS
    ```
###### CSTX ID is the output of SHA-256 hash function, whose input is the CSTX.<br/>
```sh
    cryptography:hash(sha256, CSTX)             {...}           32 Octets
```
![Alt text][cstx]

---------

## Organization

A fully decentralized open-source project under CIS license, CISL.<br/>

CIS project is maintained by Cessor Developer Committee, CDC, which is the structured organization of known or anonymous CIS developers have contributed to the project.

---------

[comment]: <> (The document references are generated on run time!)
