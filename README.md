# Overview

A distributed platform to compose and compute information services, from a simple mathematical operation to rotation of a solar cell on a spacecraft.<br/>
The distributed storage records worthful information for centuries as D20-Blockchain with consensus mechanism of PoS.<br/>
The decentralized network on top of UDP/IP routed by distributed hash table provides peer-to-peer connections, which enable nodes to stablish end-to-end secure transmission tunnel, even though they use dynamic IP addresses behind ISPs, thanks to Hole-Punching mechanism.<br/>
The distributed environment enhances developers to implement and deploy their services coded with C/C++, Erlang/OTP, JAVA, Python, JavaScript and another supported programming languages and frameworks. It provides a laboratory of distributed systems for developers to design, implement, deploy and test services for example on a local IoT system. Furthermore, web application providers are able to serve their services using the advantages of distributed computing to reduce serving costs.<br/>
Free Flow of Information and Privacy are the CIS duties. Every body can provide, access and use information services on CIS anonymously.<br/>



# Cessor Information Systems, CIS

A distributed service-oriented and TX-based platform to perform particular processor of a cessor service, CS.<br />
CS is a distributed TX-based software-service documented and coded by supported programming languages.<br />
CIS provides cessor service transactions, CSTX as the I/O medium to communicate with other CS on CIS.<br />
CSTX is the structured service information object integrated with CIS.<br />

CS composes and computes CSTX using cessor service transaction protocol, CSTP.<br />
CSTP addressed CSTX to perform particular processor of a CS on CIS.<br />
CSTP maps a set of functions in a module of a package in a specific range on an allocated CS domain.<br />

# Cessor Service Transaction Protocol, CSTP

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

![alt text][ref_cstp_layers]

# Cessor Service Transaction, CSTX
- CSTX contains three parts:

1. TX header<br/>
    - The header addresses to a unique processor on CIS.<br/>
	I. Protocol Version Control
    ```sh
        Protocol Version Control, PVC       {CBI,VSD,VSN}       3 Octets
    ```
    ```sh
    CBI = First octet is allocated for Crown-Block index, on which the protocol is issued.
    VSD = Second octet is allocated for the version serial domain.
    VSN = Third octet is allocated for the version serial number.
    *VSN <255> is reserved for version extension.
    ```

	II. Service Domain Range

    ```sh
        Service Domain Range, SDR           {SDR0,SDR1}         2 Octets
    ```
    ```sh
    SDR0 = First octet is allocated for service domain.
    SDR1 = Second octet is allocated for service sub-domain.

    *Service domain <0> has following reserved sub-domains
                        <0> .. <3>	system services

    *Service domain 255 is reserved for local service sub-domains
    ```

	III. Procedure Source
    ```sh
        Procedure Source, PS                {PS0,PS1}           2 Octets
    ```
    ```sh
    PS0 = First octet is allocated for service procedure range.
	PS1 = Second octet is allocated for service procedure package.
    ```

	IV. Process Reference

    ```sh
        Process Reference, PR               {PR0,PR1}           2 Octets
    ```
    ```sh
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
    ```
    ```sh
    K = TX authentication length
    ```
    II. TX authentication length
    ```sh
        Authentication length                   {K}             1 Octet
    ```
    ```sh
    256 > K >= 0, The length of authentication information
    ```
    III. TX authentication version
    ```sh
        Authentication version                  {Ver}           1 Octet
    ```
    ```sh
    256 > Ver >= 0, The version of authentication information on CIS
    ```
### CSTX ID is the output of SHA-256 hash function, whose input is the CSTX.<br/>
![alt text][ref_cstx]
# Organization

A fully decentralized open-source project under CIS license, CISL.<br/>

CIS project is maintained by Cessor Developer Committee, CDC, which is the structured organization of known or anonymous CIS developers have contributed to the project.





[ref_cstp_layers]: <https://github.com/cessor-org/CIS/blob/main/docs/CSTP/CSTP-layers.png>
[ref_cstx]: <https://github.com/cessor-org/CIS/blob/main/docs/CSTP/CSTX.png>