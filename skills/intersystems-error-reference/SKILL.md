# InterSystems Error Reference

General Error Messages

This document contains tables of numeric error codes and their corresponding error messages for InterSystems IRIS® data platform. Commonly, these error codes are reported as ERROR #nnn. These error codes are sometimes referred to as %Status error codes.

The $SYSTEM.Status class methods used for handling these error codes are documented in the InterSystems Class Reference.

You can determine the error message for a specified error code using the DisplayError() and Error() methods, as shown in the following example displaying error code #101, where the embedded message variables are %1="5", %2="10", and
%3="2.7":

ObjectScript

DO $SYSTEM.Status.DisplayError($SYSTEM.Status.Error(101,"5","10","2.7"))

- Two error codes, 83 and 5001, are provided to enable you to generate your own custom error messages. For details refer to the %SYSTEM.Status class in the InterSystems Class Reference.

- Two error codes, 5521 and 5540, are provided for SQLCODE errors. For details see the %SYSTEM.Error class.

For more information on using these error codes, refer to %Status Error Processing.

Table 1–1: General Error Codes - 0 to 199

Description

the volume already exists

the read of the map block failed

error writing map blk of primary volume

unable to read the global directory map block

unable to write the global directory map block

unable to write the global directory block

failed opening the next volume

failed reading the next volume's map block

Description

the directory name is too long

the number of maps is invalid

the size is out of range

failed creating a new volume

the file was already mounted

the file already exists

a file create is in progress

the current # of maps is too small

unable to expand the file

the file is cluster mounted

unable to allocate CFN

incompatible mount state or db does not exist

the system mgr's database cannot be cluster mounted

the database is in transition

the system is not part of the cluster

can't change the mode of a mounted database

there is not enough space on device for new vol

the new volume exceeds the system file size limit

unknown error writing to new volume

the database is being expanded

the database is not mountable

the database is mounted elsewhere

there is no room in GVXTAB for secondary volumes

the volume is readonly

databases cannot be deleted while they are cluster mounted

the directory was not found

The database name is invalid

the write daemon failed to set the READ/WRITE flag in label

the expansion failed to start

some or all database files were not deleted

unknown and unexpected error

invalid argument

target could not be opened

target could not be read

Description

target could not be written to

the database is being restored

the database does not exist

the operation requires too many bitmap blocks

the allocation new bitmap blks failed

the database must be dismounted to do this

the database must be privately mounted for this

global directory must be empty

cannot cluster mount temp database

cannot dismount temp database

cannot reinitialize mounted database

the resource name in the database is not known to the system

the encryption key for this database is not activated

the mounted database count exceeds license limit

read/write state of mirrored databases can only be changed on the primary

*** Error while formatting volume because

Not owner

No such raw disk device

No such directory

I/O error

No such device or address

Permission to access file denied

Device or resource busy

File already exists

No such device or inappropriate use

File table overflow

Too many open files

Read-only file system

Error code = %1

Audit Database Max size must be set to 0

Operation is not permitted when running in single user mode

the database default collation is not available

the database block size is too small to support direct I/O

direct I/O is not supported on NFS filesystems

Description

database must be opened for direct I/O because async I/O is enabled

2K database block size no longer supported

Creation of Database Extent is not allowed

Database was created in system with different endian

Journaling is required for Audit database

Top Pointer Level: # of blocks=%1 %2kb (%3% full)

Bottom Pointer Level: # of blocks=%1 %2kb (%3% full)

Pointer Level: # of blocks=%1 %2kb (%3% full)

Top/Bottom Pnt Level: # of blocks=%1 %2kb (%3% full)

Data Level: # of blocks=%1 %2kb (%3% full)

Total: # of blocks=%1 %2kb (%3% full)

Elapsed Time = %1 seconds, Completed %2

Error of type %1 while processing pointer block %2

The error occurred while processing node %1

The lower level block specifies a right link block of %1.

Error of type 1. View buffer not open or this dataset can't be mounted.

which is the first block on this level.

which has a left neighbor pointer block of %1

The pointer block is degraded and can't be parsed.

The lower level block is degraded and can't be parsed.

The global reference input as the expected first node is too long.

The pointer block's 1st node - %1 points to block %2. We were expecting it to point to %3, which is the right link of the last lower block of the previous pointer block.

The pointer block's 1st node is: %1. It does not

follow the last global reference.

equal the expected global reference based upon the right link data.

of the last lower block of the previous pointer block, which is: %1.

which is %1 pointing to the lower level block %2

**********Global %1 is Not OK**********

Global ^%1 is OK

The lower level block has a block type of %1

whereas we were expecting %1

The pointer block expected the data block to have

The pointer block did not expect the data block to have any

Description

big strings but the data block's type information

big strings but the data block's big string count

says it does not.

says it does.

The lower level block's info about the first node in the next block is wrong.

The length in blnextpntlen4 is 0 but there is a right link

The length in blnextpntlen4 is nonzero but there is no right link

The length in blnextpntlen4 is too long for a global reference.

The reference described by blnextpntlen4/blnextpntvalue4

doesn't follow the last node in the block.

The length in blnextpntlen4 does not match the length of the first node in the next block

The lower level block has a value in blnextpntlen4

The lower level block has a value in blnextpntoff44

but this isn't a data block

blnextpntoff4 but this isn't a big database data block

(discovered while looking for big strings in the block)

The data block's count of big strings is %1.

whereas its block type specifies

there should be big strings.

there should not be big strings.

The data block has a syntax error

in its big string info

Map block %1 has a label error

The lower block %1 isn't allocated in map block %2

The data block points to a big string stored in block %1

that isn't allocated from its map block %1.

The pointer block is empty.

The lower block has a right link global reference that doesn't

Match what was expected in the next pointer node's global reference.

We would expect the lower block's last node to collate earlier.

We would expect them to be equal.

The lower block's right link reference is %1

The pointer block's next reference is %1.

The pointer node's global reference doesn't match

Description

the 1st node of the lower block.

The lower block's 1st node is %1.

Since it is a big database data block it should match

the first blpntlen4 bytes of the first node, which is %1.

That doesn't match the next pointer node in the pointer block, which is %1.

The pointer node specifies a block # %1

That is out of the range of this database.

The pointer block has a right link of %1.

No longer present on disk.

Block %1 is not a pointer block type: %2

Top block %1 does not have a top pointer block type: %2

Lower level pointer block %1 has a top pointer block type: %2

Big Strings: # of blocks=%2 %3MB (%4% full) # = %1

Big Strings: # of blocks=%2 %3kb (%4% full) # = %1

The database cannot be mounted because

An unexpected error occurred: %1

Value (report to InterSystems) = %1

***Further checking of this directory is aborted.

***Further checking of this global is aborted.

***We will continue checking with the next pointer block at this level.

The database is not mounted.

Inserted new node %1 at end of block.

Consider if this node should be in this block.

Inserted new node 1 at beginning of block.

Changes are needed in other blocks.

Inserted as new node %1.

Old node %1 and subsequent nodes have been shuffled up.

Node already exists (Node %1).

*** Not enough room in block. ***

...Deleted. (Higher numbered nodes have been shuffled down.)

Deleted 1st node in block.

***Type is %1 - Invalid Type.

***Offset improper: %1 shouldn't be > %2

Top Pointer Level: # of blocks=%1 %2MB (%3% full)

Description

Bottom Pointer Level: # of blocks=%1 %2MB (%3% full)

Pointer Level: # of blocks=%1 %2MB (%3% full)

Table 1–2: General Error Codes - 200 to 399

Description

Top/Bottom Pnt Level: # of blocks=%1 %2MB (%3% full)

Data Level: # of blocks=%1 %2MB (%3% full)

Total: # of blocks=%1 %2MB (%3% full)

but the lower block has a right link of %1.

***Map Error: The count field in map block %1 says %2 but the counted total is %3.

Integrity Job failed to start.

Stop integrity check?

Stop checking directory?

Stop checking global?

This points to big string block %1 but that has type %2

The pointer block contains the wrong global

Cannot insert long strings.

Creating 2k databases not allowed.

There are %1 duplicate pointers, the first is global %2 pointing to %3.

There is a duplicate pointer, global %1 pointing to %2.

'^%1' is not a valid global name.

Global name '^%1' is too long (over %2).

The stored value of the next pointer in this data block does not match the actual next pointer or blnextpntlen4 is incorrect.

Node #%1 in block #%2 contains an invalid subscript length

Collate #%1 in the block does not match with the collate #%2 in global directory

Block offset in bigstring block #%1 has bad value #%2.

The bigstring block #%1 is pointed with a bad block value #%2.

Block #%1 changed during integrity check, counts may be incorrect

Block #%1 changed during integrity check, might be OK, check should be rerun for this global

Unable to read global directory block #%1

Invalid name %1 in global directory block #%3, entry %2

%1 block %2 is corrupt

%1 block %2 has incorrect type

Description

%1 block %2 has incorrect label

%1 block %2 has incorrect map number

%1 block %2 has incorrect incremental file number

%1 block %2 has incorrect database creation time

%1 block %2 is marked as bad

The top pointer block %1 is not marked as allocated

%1 errors found during integrity check

whereas we were expecting a pointer block type (2 or 6)

Global directory block %1 points back to previous block %2 in chain

Global directory block %1 has invalid type %2

*** Warning: Counts for global %1 may not be correct, one or more blocks changed during check ***

*** Warning: Global %1 might be corrupt, should be rechecked ***

*** Errors were detected, but only in blocks that changed during check ***

which is %1 in data block %2

which is %1 pointing to a lower level block, however, node may have been deleted. Re-run this check to be sure

The top pointer block %1 has a right link %2

Level %1 possibly has an infinite loop, should have %2 blocks, seen %3 already

Level %1 possibly has an infinite loop, pointer block %2's right link %3 seen already

That doesn't match the next pointer node in the pointer block, however, node may have been deleted. Re-run this check to be sure.

Error parsing global directory block %1. Some globals may be unavailable.

Block offset in the pointer block has a bad value.

Block offset in the lower level block has a bad value.

Error parsing pointer block %1.

%1 block %2 isn't allocated from its map block %3.

the database is not mounted.

the database has degradation.

the database is read-only.

A primary volume already exists in %1.

A secondary volume already exists in %1.

You must choose another location for this primary volume.

You must choose another location for this secondary volume.

Created %1 but failed to mount it. Mount error is %2.

Description

Global %1 not found.

Global already defined.

Temp database cannot be the Manager's database

Failed to designate %1 as the Temp database

Failed to lookup sfn for the Temp database

Modify of %1 failed because

This is not a database file %1

The database was created but not formatted.

Cannot delete %1 because

Cannot delete database.

Deleting mirrored database %1 is not allowed.

Completed reclaiming routine blocks for %1.

Create failed because: %1

Could not set Keep Type for %1 because

%1 not mounted because %2 networking is not active.

Cluster mount failed for %1.

This global directory is corrupt.

Could not set journaling type for %1 because

Could not set protection for %1 because

There are no %1 KB buffers (or bigger) configured.

Could not set collation for %1 because

Database Error in ^%1, (report to InterSystems)

Block %1 is not used in any global

Block passed is wrong length: %1

Big string block, use Block Dump option

Global %1 already exists

Cannot create global %1

Global %1 would be remote, not allowed

%1 is not a legal name

Cannot write to file %1

Cannot read input file %1, error is %2

%1 is not a good configuration file

File %1 is not available

Block number %1 is too high for this database

Description

Block %1 is not a map block

Block %1 is not the correct map block for %2

Function not supported for legacy databases

Cannot dismount manager's database

Cannot modify this parameter if database exists

Cannot set size less than current size of %1MB

Invalid Parameter for this Operation

Database must exist before adding volumes

Failed to mount %1, the reason is

Error reclaiming routine blocks for %1 :

Map block %1 is corrupt

Database in %1 created with %2 MB instead of requested %3 MB

Comm device(s) are currently in use

Database creation with block size %1 is not allowed

Database %1 cannot be mounted, resource %2 is unknown to the system

FileCompact failed: Insufficient global buffers

FileCompact failed: Compaction in progress

FileCompact: Freespace maximum reached

FileCompact failed: Freespace requested exceeds filesize

Defragmentation failed: Insufficient file space

Defragmentation failed: Insufficient space

Adding system database %1 to mirrored DB is not allowed

Cannot display contents of mirror information block

Namespace %1 is not available. Either the default database is not mounted, or you do not have permission to access it

Database must be larger than minimum allocated size

Import of global '%1' needs collation #%2, not available, skipping import of this item

Failed to return all cached free blocks in the Temp database

Failed, process id %1 has a truncation, compaction or defragmentation operation in progress in this database

The Expansion Size or Maximum Size must be an integer

Database %1 is dismounted

Database %1 is read only

Database %1 is cluster mounted

Description

Database %1 is not journaled

Journaling is not enabled

The collation of some system globals is incorrect

Namespace %1 is not available via ECP. ECP Status returned: %2

Failed: Truncation in progress

Failed: Unexpected block type encountered

Completed but skipped some blocks, details recorded in the messages log.

Stopped at an unmoveable block, details recorded in the messages log.

Unable to get directory info for global %1, Error: %2

Global names for the start node %1 and end node %2 must be the same

Global range %1 must come after %2

Database ExpansionSize %1MB is less than %2MB which is the minimum value of %3KB block size database.

%1 is not a valid operation for the Temp database

Unknown error, code %1

Failed due to backup in progress

Failed due to expansion in progress

Mirrored database has %1 KB block size in primary and there are no buffers configured for it

Mirrored database has %1 KB block size in primary and it is not allowed in local system

Unknown system name %1

Table 1–3: General Error Codes - 400 to 599

Description

, LINE:'%2=%3' at line %1

at line %1

Not enough fields

Invalid line, LINE:'%1'

No version information found in file '%1', file may be corrupt

Invalid version '%1'

Invalid parameter name '%1'

Invalid value for property '%1'

The following parameters are missing from section %1: '%2'

Cannot delete section %1

Too many fields

Description

File %1 has been edited, cannot modify from the management portal

Invalid map keyword %1

Invalid or duplicate section name %1

Duplicate line detected

Duplicate entry %1 detected

Section '[%1]' already exists

%1 %2 already exists

%1 %2 does not exist

%1 map %2 in namespace %3 does not exist

%1 map %2 in namespace %3 already exists

Cannot delete server %1, in use by the following databases: %2

Device name cannot be the same as the Alias

Data server %1 not defined

Data server %1 not allowed for system databases

Cannot delete system database %1

Namespace %1 already exists

Cannot delete database %1, in use by the following namespaces: %2

MountAtStartup, ClusterMountMode, and MountRequired not allowed for a remote server

System database cannot be cluster mounted

Required database %1 is not defined

Namespace %1 does not exist

Cannot delete system namespace %1

Required namespace %1 is not defined

[%1] section must be defined before [Databases] section

[Databases] section must be defined before [Namespaces] section

[DeviceSubTypes] section must be defined before [Devices] section

[Namespaces] section must be defined before [%1] section

These sections are missing: %1

Further processing is aborted

System does not support clustered databases

Comment length must be less than %1 characters

Comment must start with one of the comment chars '%1'

Invalid nested comment, LINE:'%1'

No end comment '%1' found

Description

Invalid routine type %1

Cannot map routine %1 when %2 already exists

%1 data server %1 already defined

You must delete mapping %1 before deleting mapping %2

Mapping %1 already exists

Cannot quiesce the system for namespace reactivation

Global mapping %1 must be defined before subscript mapping %2

Database %1 required, but could not be mounted

[config] MaxServers parameter must be increased to at least %1

Invalid namespace name

Invalid server name

Database %1 already exists

Invalid collation %1

System does not support IPv6

Invalid block size %1

Database %1 is not allowed for ECP Mirror Connection

ECP Server %1 does not exist

Remote Server %1 does not support Mirror

LongStrings cannot be enabled when 2KB or 4KB databases are mounted

Alias %1 is already in use by device %2

Invalid shadow name

[Shadows] section must be defined before [%1] section

Server name '%1' matched local system name

%1 section must be defined before [%2] section

New WIJ directory cannot contain an existing IRIS.WIJ file.

No entry found pointing at local directory %1 in the [Databases] section of the configuration

Unable to obtain lock on CPF file %1

Database %1 is required read-write, but was mounted read-only

Invalid MirrorConnection value %1

Database %1 is not allowed for ECP Non-Mirror Connection

Global %1 contains control characters. Restore of this global may fail. Use block format to save this data. See file %2 for details.

Database copy to %1 is already running

Cannot copy and replace the cluster mounted database %1

Description

Cannot copy and replace a mirrored database %1

Table 1–4: General Error Codes - 600 to 799

Description

CSP Application

Data Server

Database

Device

Global Mapping

Global Replication

License Server

Namespace

SQL Gateway

Routine Mapping

Mag Tape

Device Sub Type

Ethernet Connection

UDP Connection

Ethernet Device

Volume Set-UCI Mapping

Shadow Destination

Shadow Source

LAT Service

Com Port

SQL System Data Type

SQL User Data Type

SLM Replication

SLM

Journal History

Remote Volume Set

Namespaces

Databases

Devices

Configuration

Description

Projection type

Java Application

EJB Application

C++ Application

Class Mapping

%1 '%2' is not defined in this Configuration.

%1 '%2' is referenced by the following %3.

%1 '%2' already exists.

Parameter '%1' invalid: '%2'.

%1 '%2' does not exist.

The configuration could not be reactivated because the changes made require a restart.

Error loading configuration %1: %2.

Configuration %1 needs to be %Saved() before calling the Activate() method.

Configuration %1 is in use by another process.

%1 already defined in Namespace '%2'.

Failed to set Startup configuration to '%1'.

A clustered configuration requires a non empty PIJDirectory.

Subscript reference may not contain the '~' character.

Subscript reference must begin with an open parenthesis.

Subscript reference must end with a close parenthesis.

Open parenthesis before a close parenthesis.

Invalid subscript in reference %1 subscript #1.

Invalid subscript in reference %1 subscript #%2.

Invalid range specification.

More that two references in range specification.

Name required for setting within Config API.

Key is required.

Unable to find information for config setting: %1

[Property does not exist]

Unable to open configuration object: %1

Remote system status change failed.

Error parsing config file: %1

Reactivation error: %1

Collate #%1 entered does not match with the collate #%2 of ^%3 in global directory

Description

LDAP error(%1): %2

LDAP or passed argument is not initialized

Failed to load LDAP shared lib(%1)

Value reach 32K boundary

LDAP can't allocate enough from heap

Invalid parent

Unexpected object passed

LDAP unexpected library version - expecting - %1 loaded - %2

Server passed back another challenge, determine the response to that challenge and call the SASLConect again to send that response

Invalid parameter was passed

Request is not supported

specified SASL mechanism is not supported

Invalid peer certificate verification level for client type

Expanded CipherSuite list contains no values

SSL Communication Not Permitted With Current License

Certificate %1 has expired

Certificate %1 is not valid for TLS Web client authentication

Certificate %1 is not valid for TLS Web server authentication

Encryption with public key in certificate %1 failed

Decryption of private key file %1 failed (possible bad password)

The public key in certificate %1 and the private key in %2 do not match

Verification of certificate %1 with CA file %2 failed, error= %3

'%1' member missing mirror SSL configuration

'%1' is not a mirror member

'%1' is unreachable, error=%2

Can not disable all authentication mechanisms used by Terminal

Mirror SSL validation of '%1' failed with error: %2

Mirror SSL configuration missing certificate file name

Mirror SSL configuration missing CA file name

Certificate %1 is not valid

CA certificate file %1 is not valid

The X.509 certificate is missing

User account has expired

Description

%1 authentication failed

Invalid Application name %1

Table 1–5: General Error Codes - 800 to 999

Description

Logins for Service %1 are disabled

Logins are disabled

Logins are disabled for service %1, system startup in progress

Logins are disabled, system shutdown is in progress

Kerberos logins not allowed for service %1

Kerberos data integrity logins are not allowed for service %1

Kerberos data encryption logins not allowed for service %1

O/S logins are not allowed for service %1

Kerberos logins required for service %1

Service %1 does not exist

Invalid username or password

Kerberos K5CCache logins not allowed for service %1

Kerberos K5Prompt logins not allowed for service %1

Kerberos K5API logins not allowed for service %1

Kerberos K5KeyTab logins not allowed for service %1

User not authorized for service %1

Invalid authentication option %1

Client IP Address %1 not authorized for service %2

Cannot delete service %1

Service %1 already exists

Invalid authentication option %1 for service %2

Access Denied: Cannot access %1

Access Denied

Invalid Username or Password

Unable to initialize SQL, %1

Unable to run ZSTART, %1

User %1 is not authorized

User %1 account is disabled

User %1 unable to add role %2

Description

User %1 unable to update last login

User %1 invalid name or password

User %1 error updating password

Login timeout

Login aborted

User %1 bypassing system security

Insufficient privilege for programmer access

User %1 already exists

User %1 does not exist

Cannot delete superuser %1.

Cannot delete %1, only user with %All role.

Cannot delete default user %1.

Username %1 is invalid.

Username %1 is in use by service %2.

Insufficient privilege for namespace %1, database %2, resource %3

Password does not match length or pattern requirements

Username cannot contain domain specification

System Security configuration %1 already exists.

System Security configuration %1 does not exist.

Audit database %1 not available

Invalid Audit Event name %1

Audit Event %1 already exists

Audit Event %1 does not exist

Cannot delete system Audit Event %1

Cannot modify system Audit Event %1

Error stopping auditing to %1

Unable to start auditing to %1

Unable to quiesce system to erase audit file

Audit record %1 does not exist

Unable to initialize security label for %1, resource is %2

Privileged application %1 is disabled.

User is restricted from running privileged application %2 -- cannot execute.

Privileged application %1 is locked.

An authenticated user name is required.

Description

Routine %1, in database %2, is not authorized to add roles for application %3.

Client application %1 not authorized to add roles - Signature %2.

Cannot create privileged application %1 -- an application by that name already exists.

Privileged application %1 not found.

Application %1 does not exist

Cannot delete system application %1

Duplicate Match role %1.

Match role %1 does not exist.

Duplicate Target role %1.

Target role %1 does not exist.

Cannot delete role %1.

Cannot remove role %1.

Role %1 does not exist.

Role %1 already exists.

Maximum number of roles reached.

Cannot modify role %1.

Invalid role name %1.

Cannot delete system resource %1.

Resource %1 already exists.

Resource %1 does not exist.

Cannot modify system resource %1.

Maximum number of resources reached.

Duplicate resource %1.

Invalid resource name %1.

Invalid permission %1 for resource name %2.

SSL configuration %1 already exists

Cannot delete domain %1, domain is in use.

Domain %1 already exists.

Invalid Domain name %1

Domain %1 does not exist

User's must all be in domain %1, user %2 is not

PhoneProvider %1 does not exist

X509Credentials %1 does not exist

OpenAMIdentityServices %1 does not exist

Description

Cannot modify field '%1'

Operation requires %1 privilege

Operation requires %1 privilege on resource %2

Operation requires %1 privilege on resource %2 or %3

Operation requires %1 privilege on resources %2 and %3

Cannot delete system security parameters

Password change required.

Insufficient privilege for object access '%1'

Insufficient privilege for operation

Audit header contains unwritten records

Username and Role cannot have the same name

User %1 has no role

Invalid expirationdate

Import of audit events to namespace '%1' is prohibited

User %1 has no accessible namespaces

Password logins not allowed for service %1

Unrecognized connection message

Unable to get full header of message within timeout

Invalid service name %1

Unauthenticated access for service %1 is disabled

Invalid password

Invalid Legacy password

Invalid password, cannot convert legacy password

Invalid Kerberos username or password for user %1

Kerberos error: %1

Password logins not allowed for application %1

Invalid password pattern '%1'

User %1 account has expired

User %1 account is inactive

Kerberos Authentication Not Permitted With Current License

Cache Direct Client must be upgraded

No authentication enabled for service

LDAP server unavailable - %1 %2 %3

LDAP search bind failed, error %1, %2

Description

LDAP search failed, error %1, %2

LDAP count entries failed, error %1, %2

User %1 does not exist in the LDAP database

User %1 is not unique in the LDAP database

LDAP first entry failed, error %1, %2

Invalid LDAP password, error %1, %2

User %1 is not a LDAP user

User %1 is not a Delegated user

User %1 is not an IRIS user. They are either LDAP, Delegated, Kerberos, or O/S

LDAP Get DN failed, error %1, %2

LDAP Get Values Len failed, error %1, %2

Attribute value %1 must be in $list format

System requires that the user must own the %1 role to connect

SSL configuration %1 does not exist

Unable to activate SSL configuration %1

Invalid SSL configuration name %1

All specified CipherSuites require server authentication, Certificate File and Private Key File are required

Private Key File is required when Certificate File is specified

Certificate File is required when Private Key File is specified

Private Key File is required when Private Key Password is specified

CA File is required when Peer Verification or CRL File is specified

SSL configuration %1 is disabled

SSL handshake failed

SSL connection failed, make sure server address and port (not url) is specified

Can only test SSL Client

Host and Port must be specified

Password has expired

Cannot modify LDAP authentication user

Cannot modify Delegated authentication user

SSL/TLS is required for incoming connections

SSL/TLS is not configured for incoming connections

User %1 failed O/S delegated authentication

Login Token expired

Description

User %1 Login Token expired

Table 1–6: General Error Codes - 1000 to 1199

Description

Shadow configuration '%1' incomplete: missing source IP address or DNS name

Shadow configuration '%1' error: invalid source port number: %2

Shadow configuration '%1' incomplete: directory for storing copied journal files not specified

Shadow configuration '%1' incomplete: start point not specified

Shadow configuration '%1' error: manager directory %2 is not allowed as a shadow database

Shadow configuration '%1' incomplete: no database mappings exist

Invalid shadow ID '%1': character '~' not allowed

Shadow configuration '%1' error: cannot use %2, a primary or alternate journal directory, to store copied journal files

Shadow configuration '%1': Source databases or journal belong to different mirrors ('%2' and '%3').

Cannot resume a stopped shadow '%1'

Unable to acquire exclusive access to properties of shadow configuration '%1'

Must specify a shadow configuration ID

Shadow configuration '%1' does not exist

Shadow '%1' test failed: %2

Shadow '%1' test timed out

Shadow '%1' must NOT be running

Shadow database '%1' is also its corresponding source database

Database server and shadow server have incompatible shadow protocols: version '%1' on database server vs. version '%2' on shadow server

Database server and shadow server have incompatible journal versions: version %1 on database server vs. version %2 on shadow server

Connection denied by database server %1

Received unrecognizable version '%1' from server

Error allocating memory from Generic Memory Heap: %1

Insufficient Generic Memory Heap available for shadowing

Received unrecognizable message '%1' from server

Description

Cluster shadowing request denied: database server %1 is not part of a cluster

Cluster shadowing request denied: database server %1 is not part of the source cluster of shadowing, identified by %2

Shadowing aborted on error

Unable to job off shadow server process

Shadow '%1' is being stopped by another process

Unable to suspend shadow '%1' within %2 seconds

Requested journal file '%1' does not exist on the source

Requested file '%1' is not a valid journal file on the source

Journal file '%1' is corrupted

Error opening file %1: %2

Shadow copy %1 is ahead of source journal file %2

Invalid address %1 in journal file %2

Journal file to start or resume shadowing with is not specified - possibly as a result of the originally specified journal file name being invalid

Failed to sync database updates as one updater has died

Missing start point for cluster shadowing

Incomplete start point for cluster shadowing: %1

Shadowing is unavailable for current license

Shadow is already running

Bad checkpoint for cluster shadowing: %1

Database updates are NOT currently journaled on the source of shadowing - shadow databases may be out of sync with the source

Shadow is not suspended and therefore cannot be resumed

Shadow is not stopped and therefore cannot be started or restarted

Attempt to connect to %1 at port %2 timed out - database server is not running or network is down

TCP read timed out - remote server is not responding

Database server has disconnected - %1 to the server is aborted

Shadow server (%2) has disconnected - %1 to the server is aborted

Unable to job off routine %1

Failed to start purging as another job (PID %1) appears to be in the middle of purging shadow journal files

Purging not available to this shadow

Error getting answer: %1

Description

Error killing job (PID %1): %2

Purging aborted due to failure to sync journal

Error mounting shadow database %1 when processing journal file %3 -- subsequent updates to the source database %2 will NOT be applied to the shadow database

There is no database in %1 on the source or it is not readable

Database in %1 on the source is not currently mounted

Invalid source directory %1 - name too long or has invalid syntax

Invalid journal EOF at offset %1 of file '%2' - must traverse forward to get end position

Got fewer records than expected: last one at %1. Possible corruption in %2 or its source copy.

Failed to open journal file '%1' for record reading

File '%1' does not exist

File '%1' is not a valid journal file

Error getting previous file of '%1': %2

Failed to create an instance of journal file '%1'

The first record of journal file '%1' is invalid

Error deleting journal file '%1': %2

Search string not specified

Journal file not specified

Journal file '%1' is expected to be followed by another file, which does not exist

No valid record in journal file '%1'

Error getting the file following journal file '%1': %2

Corruption between offsets %2 and %3 of journal file '%1'

File '%1' does not exist in journal log '%2'

Unknown column: %1

Bad directory in journal record

Bad global node in journal record

Error starting journaling: %1

Error stopping journaling: %1

Error switching journal file: %1

Directory '%1' does not exist

Directory name '%1' is invalid

Error creating directory '%1': %2

Journal file prefix '%1' is invalid

Description

Directory name '%1' is too long for journal files with names in the form of '%2YYYYMMDD.nnn'

Commas are NOT allowed in a journal file path ('%1%2YYYYMMDD.nnn')

Unable to get directory attributes for directory %1

Journal directory %1 cannot be readonly

Invalid transaction ID: %1

Transaction start at offset %1 of file %2 is not a TSTART record

Cluster journal marker file missing

Failed to open cluster journal marker file: %1

Database encryption key activation at startup must be enabled before journal encryption can be enabled

Failed to switch journal file to activate journal encryption immediately -- journal files will be encrypted following current file

Failed to switch journal file to deactivate journal encryption immediately -- journal files will stop being encrypted following current file

Table 1–7: General Error Codes - 1200 to 1399

Description

Encryption key '%1' is already activated

Encryption key is not activated

'%1' is not a valid encryption key file

Encryption key in file '%1' does not match activated key

User '%1' not found in encryption key file '%2'

User '%1' already exists in encryption key file '%2'

Encryption key creation failed

Encryption key activation failed

Can not deactivate encryption key. Encrypted databases are mounted: %1

Invalid password. Must contain at least %1 characters

Can not remove last administrator from key file

Wide Unicode characters are not supported in administrator usernames or passwords

Disabling encryption key activation at startup is NOT allowed when %1

Disabling encryption key activation at startup is NOT allowed when the encrypted journal file '%1' is required for crash recovery

Deactivating encryption key is NOT allowed when %1

Deactivating encryption key is NOT allowed when the encrypted journal file '%1' contains open transactions

Encryption key activation at startup is still enabled

Description

Can not disable encryption key activation at startup. Encrypted databases are required at startup: %1

Encryption key activation at startup must be enabled before audit encryption can be enabled

Encryption key unwrap failed; possible incorrect password

No space available for encryption key

Key '%1' not found in encryption key file '%2'

Cannot remove unattended activation administrator from key file

Failed to lock DataCheck system

Cannot be run from DataCheck daemon job

DataCheck System already started

Global reference %1 does not collate before %2 in collation %3

DataCheck protocol error

Failed to initialize DataCheck message queue

Timed out starting DataCheck job

Failure during DataCheck job initialization

Access denied by peer with message: %1

Timed out waiting for peer

Duplicate database mapping for %1

Timed out trying to establish a connection

%1 is not supported by the peer system

Unable to detect Mirror-based DataCheck configuration using Mirror name '%1'

DataCheck source system found for destination GUID does not match the connecting destination system

Related object '%1' has an incorrect DataCheck system name (%2)

Database specification %1 is invalid

Global selection mask is defined for duplicate database specifications %1 and %2

RangeList state is invalid due to previous error and must be reloaded

RangeList Collation is already set

RangeList has newer version stored and must be reloaded

Initial global reference is null

Initial global reference and target global reference are identical

Initial global reference and target global reference refer to different global names

Workflow must have at least one phase

Workflow NextPhase is out of range

Description

Global Reference is invalid

Table 1–8: General Error Codes - 1400 to 1599

Description

User %1 is not a Kerberos user

Routine ZAUTHORIZE not found, see the ZAUTHORIZE routine in the SAMPLES namespace

Routine ZAUTHENTICATE not found, see the ZAUTHENTICATE routine in the SAMPLES namespace

Routine ZAUTHENTICATE requires the following parameters:
(ServiceName,Namespace,Username,Password,.Properties), see the ZAUTHENTICATE routine in the SAMPLES namespace

Cannot modify Kerberos authentication user

User %1 is not a O/S user

Routine ZAUTHORIZE requires the following parameters:
(ServiceName,Namespace,Username,Password,.Credentials,.Properties), see the ZAUTHORIZE routine in the SAMPLES namespace

Cannot modify O/S authentication user

Invalid authentication option %1 for application %2

User '%1' is not configured for two-factor authentication

Incorrect function code '%1' for two-factor authentication

Two-factor authentication timeout

Incorrect token received for two-factor authentication

Mobile phone service provider '%1' already exists

Mobile phone service provider '%1' does not exist

User '%1' has invalid mobile phone number '%2'

User '%1' has invalid mobile phone service provider '%2'

Invalid configuration for two-factor authentication

User %1 account has reached the invalid login limit

GetCredentials^ZAUTHENTICATE failed

GetCredentials^ZAUTHENTICATE failed to return a username or password

User '%1' has mobile phone number but no service provider

Mirror and shadow service cannot both be enabled.

LoginRules Security configuration %1 does not exist.

Cannot add roles while ZINSERT active

You must modify settings through the Security.System class

Unauthenticated access for application %1 is disabled

Description

Unable to add or set Audit Event %1, event table may be full

Studio does not support Two Factor Authentication

Insufficient privilege for service %1

Cannot use implied namespace %1 with -U switch

The %Manager role requires the %1 resource with R/W access

Domain name for user %1 is NULL, check network configuration or Kerberos settings

Incorrect verification code received for one time password authentication

Two-factor Authentication requires one of the following to be enabled: %1

Two-factor SMS text authentication requires a phone number and service provider

Only one type of Two-factor authentication can be enabled

SSL configuration '%1' does not include valid SSL certificate

Management Portal unavailable.You must modify settings through the System Management menu.

Table 1–9: General Error Codes - 1600 to 1699

Description

%1 already exists

%1 does not exist

%1 is still running, Pid: %1

Cannot modify %1, operation is either running or has completed

User terminated the operation

Cannot open journal file %1

Unable to find journal file after %1

Source and destination database are the same - %1

Cannot copy from an ECP database

Cannot copy from system database %1

Journaling must be enabled on your system

Journaling is troubled - %1

Database %1 - %2 must be configured for mirroring

Unable to set mirror failover state, Status - %1

Nodes from global %1 already exist in destination database %2

Global %1 does not exist in source database %2

%1 has already been run

Cannot operate on namespace %1

Routines are already split from namespace %1

Description

Unable to get directory info for global %1, Error: %2

Collation for global %1 in directories %2 and %3 don't match

Move globals job failed to start.

Move globals %1 has already been run.

Operation terminated by error

Starting and ending global must be the same

State is wrong for %1 call, State=%2

Unable to obtain Data Move Namespace lock

Unable to find SFN in journal %1 for source directory %2

Collation of global %1 has changed, unrecoverable error at journal offset %2

Did not handle Kill of node %1 - Range = %2

UnHandled Journal record type %1 at journal offset %2

Unable to start journal monitor

Cannot calculate Freespace for %1, Database is dismounted

Max size of database %1 must be increased by at least %2 MB

Move of data into the following databases exceeds the space on the partition by %1 MB:
%2

New destination database directory for database %1 is not configured

New destination database directory %1 for database %2 is already configured as database %3

The following globals in range %1 already exist in destination database %2 : %3

Data Move Operation %1 is running

Unable to find and delete range %1

Invalid method %1

Invalid Job method %1

Cannot Job method %1

Move Data operation %1 %2 is already running

Move operation %1 has already completed

Move operation %1 has an unrecoverable error, you must roll it back

Collation of global %1 has changed, unrecoverable error

Unable to quiesce system within %1 seconds

Unable to Suspend, operation already completed

Unable to Suspend, operation not started

Unable to Suspend, operation past journal phase

Description

Unable to suspend, operation already stopped

Unable to Stop, operation already completed

Unable to Stop, operation not started

Unable to Stop, operation past journal phase

Invalid Data Move name %1

Table 1–10: General Error Codes - 2000 to 2299

Description

Journal file #%1 for database '%2' not found in mirror journal log (%3)

Failed to read header of journal file '%1'

Mirror name not specified

Mirror journal log file '%1' not found

Failed to open journal log for mirror '%1'

Failed to read journal log for mirror '%1'

Cannot modify the name of the mirror set

Cannot modify the GUID associated with the mirror set

Failed to send updated recovery parameters to mirror members

Mirror set GUID is not defined. %1 section failed to load

Failed to load mirror configuration

Mirror name cannot contain the ':' character

Mirror name exceeds the maximum length of %1 characters

Mirror parameters are already loaded - cannot be reloaded with MirrorMember.Load()

JoinMirror and AsyncMemberGUID should not both be set - %1 aborting

JoinMirror and AsyncMemberGUID should not both be set

Missing system name in [MirrorMember] section, can't join mirror

Missing mirror name in [MirrorMember] section, can't join mirror

Missing mirror GUID in [MirrorMember] section, can't join mirror

MirrorMember.CheckSecurity failed to open mirror service '%1'

Mirror name not defined

Bad mirror name '%1'

Cannot shutdown mirroring on the primary mirror member

Description

System name cannot contain the ':' character

System name exceeds the maximum length of %1 characters

Mirror name not configured, AsyncMemberAuthorizedIDs cannot be loaded

SSL DN (Distinguished Name) field already in use

SSL DN (Distinguished Name) field cannot be null

%1 missing required parameter(s) - aborting

No Async member configuration is defined

No mirror set name to update

Could not find mirror set %1 in query list

Mirror set name %1 does not exist

Invalid mirror configuration for %1, system name '%2' is not unique

Failed to allocate mirror set %1 structure

Found duplicate mirror name or GUID with local in %1

Failed to load mirror configuration of '%1'

Failed to retrieve mirror configuration for %1 from %2 (%3)

Mirror member name cannot contain the ':' character

Mirror member name exceeds the maximum length of %1 characters

Mirror set name is not defined

Load All Mirror Set Members already run once, cannot be executed again

Failed to find our mirror name (%1) in the mirror configuration for %2

Could not add new member when the Async member connected

Argument to %1 is not an object

Failed to add Mirror Set Member %1

Invalid mirror configuration for %1, guid (%2) for system %3 is not unique

Incorrect base directory '%1' - Expected '%2'

Failed to add Mirror Set Member %1 (#%2) to mirror %3

Insufficient privilege to startup mirroring

Mirror configuration not loaded

Mirror set name '%1' is not configured

Failed to start mirror manager daemon %1

Failed to create mirror journal log file '%1'

Failed to delete mirror journal log file '%1'

Failed to open mirror journal file (%1) containing the start point of the journal file purge

Virtual IP for mirror %1 is not a valid address '%2'

Description

Interface of Mirror Virtual IP does not exist '%1'

Mirror Database Name is required but not provided

Mirror Database Name exceeds the maximum length of %1 characters

Mirror Database Name cannot contain the ':' character

Mirror Database Name '%1' is not unique, found in mirror member %2

Failed to check other systems for duplicate Mirror Database name

Database '%1' is already being mirrored

Cannot remove database '%1' as is not currently being mirrored

Could not create new mirror: %1

Mirroring Service is required but not enabled

SSL Configuration %1 is required but missing

SSL Configuration %1 is not enabled

Names in SSL Server Configuration '%1' and Client Configuration '%2' are different

Mirror Virtual IP '%1' is owned by another system

Error retrieving Mirror Set information for '%1'. Error info: %2

The character size of the other system is different from local system

Mirrored Database '%1' is not found on this system

This system has not been configured as a Mirror Member

There is no other failover member defined on this system

Error retrieving Mirror Member information for '%1'. Error info: %2

Failed to force this member become primary, reason: %1

Mirrored DB is already activated

Failed to activate mirrored DB reason: %1

Failed to remove mirrored DB reason: %1

This is not a Failover mirror member

Failed to connect to mirror primary node

Failed to lookup instance name, reason: %1

Could not join existing mirror: %1

Virtual IP did not include or had bad CIDR subnet mask: %1

Agent is unreachable with %1, reason: %2

Mirror member %1 is unreachable with %2

ECP connection to Mirror member %1 is unreachable with %2

Network Interface %1 is not a virtual interface

Network Interface is not specified for Virtual Address

Description

Failed to get SSL DN field on %1, reason: %2

SSL required to mirror encrypted database

Insufficient privilege to shutdown mirroring

Mirror connections for %1 failed to disconnect cleanly

Problem detected with mirror SSL/TLS configuration

Mirror Virtual IP '%1' is not reachable

Mirror Virtual IP '%1' could not find a matched subnet in Interface '%2'

Cannot find starting location from filename '%1'

Mirroring is unavailable for current license

Failed to open MirrorSetMember entry for %1 (%2)

Mirror name '%1' is already in use

Mirrored DB %1 not found in failover member

Mirrored DB %1 not found in primary member

Failed to create new mirrored DB (%1)

Matching mirrored DB %1 in member %2 was not created as mirrored DB

Mirror Set %1 has already been started

Mirror Set %1 has not been started

Failed to open [Mirrors] entry for %1 (%2)

Failover members can only be a member of a single mirror. [Mirrors] contains %1 mirror definitions

Mirror name '%1' is not valid - must contain only alphanumeric characters

Failed to read local mirror member information (%1)

Delete operations on %1 are not permitted

Operation can only be performed on the primary mirror member

Failed to open [MapMirrors.%1] entry for %2

Clear FailoverDB Flag is not allowed on this system

Operation is not allowed on the primary mirror member

Mirror promotion is not allowed for non DR member

ISCAgent is not up on the local system

Mirror promotion is only allowed when only one mirror set is configured

Mirror promotion is only allowed when only one mirror set is configured

One of the failover members is unreachable through the ISCAgent

Failed to create Config.MapMirrors object

GUID mismatch in journal files from mirror members (%1) vs (%2)

Description

Selected mirror partner %1 is not in the failover member list

Selected mirror partner %1 is not a primary candidate

Failed to clear ValidatedMember on %1. Error: %2

Failed to tell primary %1 to promote %2. Error: %3

ISCAgent is unreachable

Journaling is required for mirrored databases

Cannot remove database '%1' as it is not currently mounted

Mirror promotion is not allowed for a relay server member

VIP is configured but network interface is not configured

Demotion is not allowed when this is the only failover member

This member cannot belong to more than one mirror

Failed to get ISCAgent version information

Instance's version is later than ISCAgent's version

There are already more than one failover member configured

Remote member has different UNICODE property from local member

Passed system name %1 is different from configured system name %2

AsyncMemberType parameter is out of range or mismatch with the current setting

Clear FailoverDB Flag is not allowed on non-activated mirrored DB

Clear FailoverDB Flag failed due to: %1

Default system name exceeds the maximum length of %1 characters, caller needs to provide a system name

A DR async member cannot belong to more than one mirror

DR async member is not allowed to connect to a non-failover member

Journal encryption is not allowed when Mirror's UseSSL is not enabled

Journal encryption is enabled but Mirror's UseSSL is not enabled

This member is not a reporting member

Dejournaling is already running (process id: %1)

Demote without partner does not allow running primary member '%1'.

Demote %1 failed during promotion. Error: %2.

Primary (%1) is in trouble state (%2) mirror promotion is not allowed.

This operation is only allowed on a reporting async member

This instance is not tracking any mirrors

This instance is not tracking mirror '%1'

The mirror name must be specified because the async member is tracking multiple mirrors

Description

Failed to identify the ISCAgent application server port.

Failed to identify the ISCAgent application server interface.

The ISCAgent returned an invalid status response.

This member is not an async member

his operation is only allowed on an async member

Invalid Mirror Database Name

Backup daemon did not exit after mirror shutdown

Mirror master daemon of mirror set %1 did not exit after 5 seconds timeout

Mirror Dejournal Filter is enabled but the RunFilter method in SYS.MirrorDejournal.%1 class does not exist

Mirror name '%1' contains an illegal character sequence '%2'

Non-FailoverDB mirrored database is not allowed to be configured in DR member

The mirror configuration change to %1 is blocked until the local validation trouble is resolved

Failed to create or join mirror set 5 because a mirror journal file (10) exists with the same mirror name

Mirror SSL DN is too long (over 1024 characters)

Promotion is not allowed when the mirror is in 'No Partner In No Failover' state.

Join as Failover is not allowed when the mirror is in 'No Partner In No Failover' state.

Someone else is doing Promotion or Demotion on this member.

Failed to shutdown mirror.

There is no Certificate Authority server configured at instance %1 on node %2.

Certificate Signing Request %1 not found.

Certificate number %1 not found.

Private key file %1 not found.

Certificate Signing Request %1 creation failed. OpenSSL command output: %2

Certificate %1 creation failed. OpenSSL command output: %2

Subject Distinguished Name is required.

Private Key file password is required.

Table 1–11: General Error Codes - 5000 to 5199

Description

%1

Description

ObjectScript error: %1

Not implemented

Cannot generate UUID

Cannot open file '%1'

File name '%1' is invalid

Directory name '%1' is invalid

File name is required

Directory name is required

File '%1' is already opened

File '%1' is not opened

File '%1' does not exist

Cannot Generate Type Library

%1 is not supported in this version

Namespace '%1' does not exist

Too many errors

Routine '%1' does not exist

Cannot delete file '%1'

Cannot rename file '%1'

Directory '%1' does not exist.

Expected Data is missing

Java Gateway Error: %1

Unable to copy file '%1' to '%2'

Invalid Connection Name: '%1'

Invalid ECP client action type: %1

File '%1' already exists

Invalid routine name

Unable to kill process %1

An error occurred while compiling class %1

Cannot JOB routine %1

Cannot create directory '%1'

Interrupt

Invalid status code structure (%1)

General exception Name '%1' Code '%2' Data '%3'

Failed to acquire lock on SMP Query History metadata

Description

No permission to view files in directory '%1'.

An error occurred while compiling the generator routine '%1'.

An error occurred while calling function '%1'.

Unable to copy file %1 to %2

Unable to execute Java using '%1'. Java may not be installed correctly on your system.

Unable to execute $zf(%1,%2).

Jar file %1 does not exist.

Java Exception: %1.

Java unknown error: %1.

Error executing java command '%1'. Java may not be installed correctly on your system.

Parameter '%1' marked as base64 encoded but not valid base64 '%2'.

Constraint name '%1' is invalid

Class '%1' already exists

Duplicated name: %1

Class name '%1' is invalid

Method name '%1' is invalid

Parameter name '%1' is invalid

Property name '%1' is invalid

Storage name '%1' is invalid

Trigger name '%1' is invalid

Method name conflict: %1

Parameter name conflict: %1

Property name conflict: %1

Storage name conflict: %1

Trigger name conflict: %1

Key name '%1' is invalid

Key name conflict: %1

Index name '%1' is invalid

Index name conflict: %1

Query name '%1' is invalid

Query name conflict: %1

Class name conflict: %1

Constraint name conflict: '%1'

Constraint SQL name conflict: '%1'

Description

XML Map name conflict: %1

XML Map name '%1' is invalid

Class dictionary out of date, please run upgrade utility $system.OBJ.Upgrade()

Key name '%1' is longer than '%2' characters

Index name '%1' is longer than '%2' characters

Method name '%1' is longer than '%2' characters

Property name '%1' is longer than '%2' characters

Parameter name '%1' is longer than '%2' characters

Query name '%1' is longer than '%2' characters

Storage name '%1' is longer than '%2' characters

Stored procedure name is not unique: %1, projected from %2

Package name '%1' is invalid

Package name '%1' is longer than '%2' characters

Method implementation > 32k

Projection class type is required for %1:%2.

Projection class defined for %1:%2 does not exist.

Projection class defined for %1:%2 is not a subclass of %Projection.AbstractProjection.

An error has occurred while creating projection %1:%2.

An error has occurred while removing projection %1:%2.

Name conflict on class '%1' because class '%2' has the same name but differs in case.

Name conflict on class '%1' because package '%2' has the same name but differs in case.

Member name conflict in class '%1' between '%2' and '%3'.

Name conflict on class '%1' because class '%2' could conflict in the class descriptor.

Classname '%1' is longer than %2 characters.

Collation for property '%1' is invalid: '%2'

Constraint name '%1' is longer than '%2' characters

Name conflict with project '%1' because you are trying to save project '%2' which has the same name but differs in case.

In class '%1' sqlname name '%2' from query '%3' conflicts with query '%4' sqlname '%5'.

Class name required

Environment keyword required

Method name required

Parameter name required

Property name required

Description

Storage keyword required

Storage name required

Trigger name required

Library name required

Query name required

Key name required

Index name required

XML Map name required

Package name required

Class dictionary version number in database '%1' is too high.

Class dictionary version for '%1' is out of date, please run upgrade utility
$system.OBJ.Upgrade()

In class '%1' element type '%2', element '%3' and '%4' have the same name but differ in case.

Schema name conflict on class '%1' because package '%2' has the same schema but is a different name.

The classname '%1' conflicts with the default resultset package name '%2'.

The class descriptor is too large, instance methods %1, class methods %2, instance composite %3, class composite %4, properties %5, parameters %6.

Parameter value for parameter '%1' is longer than '%2' characters

Class '%1' index '%2': the SQLNAME '%3' is not unique

Unable to find entry point for method '%1' in routine '%2'

In class '%1' alias property '%2' from property '%3' conflicts with property '%4'.

Invalid XML export version '%1', must be major minor version e.g. 2010.1.

XML export version '%1' not supported, supports 2010.1 and onwards.

In XML export keyword '%1' in class '%2' not available in target version '%3'. Will remove keyword in exported file.

In XML export keyword SqlCategory value '%1' in class '%2' not supported in version '%3'. Will remove keyword in exported file.

Invalid control character in class definition for XML export. Stripping value in XML export, value is '%1'.

Member '%1' in class '%2' contains an invalid character, the following are not valid '%3'.

Query class depends on '%1' which has been recompiled.

Parameter '%1' in class '%2' is not a CONFIGVALUE type so can not be changed.

Parameter '%1' in class '%2' is not defined in this subclass so can not be changed here, modify in superclass where it is defined.

Description

In XML export for index '%1' in class '%2' the index type=collatedkey is not supported in version '%3', will remove type keyword in exported file.

An MVENABLED persistent class does not support polymorphic dispatch so you can not create a subclass '%1' to the extent root class '%2'.

You can not have an MV enabled class with a property '%1' list/array collection of objects that includes classname.

In class '%1' alias property '%2' from property '%3' conflicts with alias property '%4' from property '%5'.

%1 keyword '%2' type in '%3' is invalid

%1 keyword '%2' value in '%3' is invalid

Class attribute keyword '%1' is invalid

Environment keyword '%1' is invalid

Method attribute keyword '%1' is invalid

Parameter attribute keyword '%1' is invalid

Property attribute keyword '%1' is invalid

Trigger attribute keyword '%1' is invalid

Class keyword type '%1' is invalid

Method keyword type '%1' is invalid

Parameter keyword type '%1' is invalid

Property keyword type '%1' is invalid

Trigger keyword type '%1' is invalid

Method keyword value '%1' is invalid

property keyword value '%1' is invalid

Key attribute keyword '%1' is invalid

Key keyword type '%1' is invalid

Key keyword value '%1' is invalid

Index attribute keyword '%1' is invalid

Index keyword type '%1' is invalid

Index keyword value '%1' is invalid

Query attribute keyword '%1' is invalid

Query keyword type '%1' is invalid

Query keyword value '%1' is invalid

Property '%1' SQL column must be greater than 1 and not greater than 4096

XML Map attribute keyword '%1' is invalid

XML Map keyword type '%1' is invalid

Description

Class keyword value '%1' is invalid

Index property collation of '%2' is invalid: '%1'

Index data property '%2' is invalid or transient: '%1'

Property '%1' SQL column must be unique: '%2' is assigned to '%3'

InitialExpression is not supported for streams, property '%1'

Can not implement a system method in class '%1'

Table 1–12: General Error Codes - 5200 to 5399

Description

Invalid parse tree

Nothing to compile

CDL Parser error: %1

Index '%1':'%2' type class, '%3' is not an INDEX class

Cannot change final method '%1'

Cannot change final parameter '%1'

Cannot change final property '%1'

Cannot inherit from final class '%1'

Cannot override final property method '%1'

Cannot replace final behavior '%1'

Cannot override key definition '%1'

Cannot override index definition '%1'

Query type cannot be changed: '%1'

Cannot change final query '%1'

Cannot override final query method '%1'

Cannot project query with parameters '%1' as view

Cannot project non-SQL query '%1' as view

Property %1: SQLComputeOnChange attribute %2 is not defined

Final keyword '%1' can not be changed

Multiple dependent relationships defined: '%1'

Cannot change final XML Map '%1'

Cannot override final XML method '%1'

Cannot override final method '%1'

There is a composite method name conflict between '%1' and '%2'

Cannot override '%1' definition: '%2'

Description

Cannot change final '%1': '%2'

Aliased method loop detected in %1:%2

Aliased method '%3' not found in %1:%2 (%4)

Aliased method '%3' signature mismatch to %1:%2

Aliased method '%1:%2' refers to class %3 that is not a superclass

Cannot introduce dependent (parent) relationship '%1' in subextent '%2' of '%3'

VERSIONPROPERTY property '%2' is not defined in '%1'

VERSIONPROPERTY cannot be changed in subextent '%1'

Cannot support calculated collection property '%1' (it can be computed but not calculated).

Class has multiple identity properties: '%1::%2'

Identity property cannot be a collection: '%1::%2'

Identity property type must be integer: '%1::%2'

IDKEY index based on non-identity property: '%1::%2'

Property '%1' is SQLComputed but no SQLComputeCode is defined

Cannot override classtype '%1' from class '%2' with '%3' in class '%4'.

Class contains too many properties and hence too many instance variables to compile.

Compilation of queued classes skipped because queued classes can not queue more classes for compilation more than twice. Classes skipped: '%1'

Unable to construct the compile tree because class '%1' which it depends on has not had inheritance resolved.

Class contains too many '%1' members '%2' maximum supported is '%3'.

Class inheritance depth is too large, maximum supported is '%1'.

Class/es '%1' has already been compiled twice during this compile so they can not be queued to compile again.

Method '%1' is missing call tag

Method '%1' is missing code

Method '%1' is missing expression

Method '%1' is missing generator

Method '%1' is missing name

Parameter '%1' is missing name

Property '%1' is missing name

Query '%1' is missing name

Query '%1' is missing type

SQL Procedure Method '%1' must be a class method

SQL Procedure Method '%1' context parameter is invalid

Description

Constraint '%1' is missing name

Projection '%1' is missing type

Method '%1' inherited from class '%2' and required to be regenerated in this subclass has
no code as superclass is deployed.

Member '%1' method '%2' inherited from class '%3' and required to be regenerated in this subclass has no code as superclass is deployed.

Class dependency loop for classes '%1'

Class dependency loop for class '%1', parent/child class '%2' has a different system level

Class dependency loop in classes that must be fully compiled before others in classes '%1'

The type of a property in a serial class cannot be recursive: %1

Class '%1' has more than one property of type %Library.RowVersion. Only one is allowed.
Properties: %2

Relationship OnDelete value '%3' in '%1':'%2' is invalid

OnDelete keyword value '%3' is only valid for a relationship: '%1':'%2'

Collection of type='%3' is not supported: '%1':'%2'

Class '%1' can not be locked for exclusive use as user '%2' in process '%3' has an escalated
lock.

Class '%1' does not exist

Class '%1' is not up-to-date

Class dependency for class '%1' is unresolved.

Circular inheritance detected: %1

Method generator dependency unresolved: %1

Compiled storage class '%1' does not exist

Class dependency for class '%1' is unresolved because its parent/child, class '%2', is
unresolved.

Method with language = '%1' cannot be projected as an SQL procedure: '%2'

Language type = '%1' not supported for method generator = '%2'

Class '%1' is a stub name and can not be opened

Attempt to set method '%1' but member method '%2' is already defined

Attempt to set member method %1:%2:%3 but this is not defined in this class

Attempt to set member method %1:%2:%3 but this is overridden by method %4

Class '%1', used by '%2', is not defined.

Name for table projected from collection '%1::%2' is not unique: %3

Routine placement dependency unresolved: %1

System shutting down so unable to compile.

Description

Class '%1' is currently being compiled by process '%2'

Method generator '%1' does not exist

Class '%1' can not be locked for shared use

Class '%1' can not be locked for exclusive use

Class '%1', used by '%2', does not exist

Internal error attempting to create class descriptor in method '%1'. Contact support

You can not use an instance property '%1' in a class method

Method or Property '%1' does not exist in this class.

You are attempting to call instance method '%1' from a class method

Class '%1' is in deployed mode.

Can not compile class in deployed mode: '%1'.

Class '%1', used by '%2', is in deployed mode.

Can not export class in deployed mode: '%1'.

Can not edit class in deployed mode: '%1'.

Only SQL DATA Map can be overridden: '%1'.

SQL Map keywords are final, only new DATA items are valid: '%1'.

SQL Map DATA piece %3 in node %2 is already used: '%1'.

Method '%1' does not exist in any superclass to class '%2'.

Method '%1' is abstract in the superclass to class '%2' so you can not call it.

You do not have write permission on the database class '%1' is in, so class lock can not be obtained.

Method '%1' is an instance method that uses ##super to call class '%2', but this class is not
a primary superclass of '%3' so can not be called.

Class dependency for class '%1' is unresolved because its predecessor, class '%2', is
unresolved.

Class dependency for class '%1' is unresolved because of the following error: %2.

No such method '%1' defined in this class.

You can not reference a property '%1' in a class method.

Class '%1' depends on class '%2' which has a different System level that prevents it being
compiled first or together.

Invalid routine to call from class '%1' to method '%2' via label '%3'.

Class descriptor for class '%1' is too large to be supported by system code.

You do not have write permission on the database item '%1' is in so unable to compile this item.

Lock table full: Class '%1' can not be locked for exclusive use

Description

Can not compile class '%1' because class '%2' is not up-to-date

Table 1–13: General Error Codes - 5400 to 5599

Description

Property cannot be stored in multiple data locations: '%1.%2'

Invalid action type: %1

Invalid CacheDirect map

Invalid CLIENTDATATYPE: %1

Invalid code mode returned by generator: %1

Invalid collection type: %1

Invalid default storage environment

Invalid ID Cardinality: %1

Invalid ID Counter: %1

Invalid ID Dependency: %1

Invalid ID Key: %1

Invalid ID Key column: %1

Invalid ID Key property: %1

Invalid identity type: %1

Invalid index attribute: %1

Invalid key

Invalid key property: %1

Invalid method code mode: %1

Invalid property type: %1

Invalid reference type: %1

Invalid storage alias

Invalid storage definition

Invalid usage of no context: %1

No data maps defined

No storage name specified

Property parameter not declared: %1

Property type can not be changed: %1

Error compiling routine: %1

Storage class not specified

Storage '%1' not defined

Description

Trigger '%1' not defined

Query parameter not declared: %1

Type specified in ROWSPEC is invalid: %1

Invalid ODBCTYPE: %1

Invalid SQLCATEGORY: %1

Invalid storage structure

Invalid storage dependency

Invalid storage literal expression: %1

Invalid storage symbol expression: %1

Storage undefined symbol: %1

Invalid serial dependency

Undefined storage symbol: %1

Data subscript already in use: %1

Multiple Id Keys defined: %1

Multiple Primary Keys defined: %1

Multiple Extent indices defined: %1

Id Key cannot be conditional: %1

Primary Key cannot be conditional: %1

Extent index cannot be conditional: %1

Cannot cluster data with Id Key: %1

Cannot cluster data with Extent index: %1

Properties cannot be defined for Extent index: %1

Extent index cannot also be a key: %1

Datatype classes can not have properties: %1

Attribute specified in EXTENTQUERYSPEC is invalid: %1

Trigger '%1' event invalid

Trigger '%1' event required

Trigger '%1' time invalid

Trigger '%1' time required

Trigger '%1' order required

Trigger '%1' code required

Stream type for attribute '%1' is invalid

Stream storage value for '%1' is invalid

Invalid foreign key attribute: '%1'

Description

Foreign key '%1' target class '%2' is invalid

Foreign key '%1' target key '%2' is invalid

Error code '%1' is out of range

Error name '%1' is invalid

Index '%1' TYPE is invalid

View classes can not have properties: %1

Id, Primary Key and Unique indices cannot override collation: %1

Bitmap index cannot be unique: %1

Cannot cluster data with a bitmap index: %1

Constraint parameter not declared: %1

ID Counter is not valid for external table: %1

Error compiling routine: %1. Errors: %2

Compilation signature in routine '%1' is incorrect

Keyword signature error in %1, keyword '%2' must be '%3'

Keyword signature error in %1, keyword '%2' must be '%3' or its subclass

An IDKEY Index is required for persistent classes: %1

%1 parameter not declared: %2

Class %1 storage definition is invalid

Class %1 storage is invalid

Invalid collection type for subnode: %1

Bitmap indices not supported in dependent class

Bitmap indices are only supported when the IDKEY is based on a single positive integer attribute

Invalid method language: %1

Invalid ROWSPEC format %2: %1

Invalid %1 formalspec format %2, expected %3

Error $ZE='%1' reported while running generator for property method '%2:%3'

Error $ZE='%1' reported while running generator for method '%2'

Cannot form a relationship with a serial or literal class, '%1'

Relationship cardinality is invalid, '%1'

Relationship cardinality is required, '%1'

Inverse cardinality, '%2' is not valid, '%1'

Relationship inverse is required, '%1'

Inverse property, '%2', is not defined, '%1'

Description

Inverse of inverse property, '%2' does not reference relationship, '%1'

Related class, '%2', has not been compiled, '%1'

Internal relationship error

%1 formal argument type in %2 is invalid: %3

Error compiling SQL Table '%1'

Field name is invalid: %1

Parent column '%1' is invalid

SQL Table, '%1', parent is invalid

SQL Counter '%1' is invalid

SQL Identity table '%1' is invalid

SQL Map data field '%1' in Map '%2' is invalid

SQL Map row IDField '%1' is invalid

SQL Map Subscript '%1' in Map '%2' is invalid

SQL Map type '%1' is invalid

SQL Reference target '%1' is invalid

Map Data Field '%1' is not a valid field

Map expression - unknown or invalid field: %1

Table '%1' already exists

Table '%1' does not exist

Table not found

Table ID '%1' does not exist

Invalid SQL Parent table

Invalid table reference

SQLError: SQLCODE=%1 %msg=%2

Cannot export SQL Table '%1', parent not exported

Table name is invalid: %1

Invalid {Field} reference in %2: '%1'

Class with View named '%1' not found

Table '%1', specified as reference by '%2', does not exist

SQL Privilege Violation

Illegal Regular SQL identifier: '%1', SQL Delimited Identifier option is off

Illegal Regular SQL identifier: '%1' is an SQL Reserved word please specify a different SQL name for this %2

Invalid username/password

Description

SQLMGR Missing class name.

Connection Error

Allocation Error

Columns error

Tables error

PrimaryKeys error

Unable to move to offset %1 in stream

Map Data Variable '%1' expression in Map '%2' is missing

Map Data Variable name missing in Map '%1', subscript level '%2'

SQLCODE: %1 Message: %2

Map: %2 - Map Expression - unknown or invalid field: %1

Map: %2 - Data Access Expression - invalid expression '%1'. Must be a {Li}, {Di}, or {iDj}
reference from a previous subscript level.

Map: %2 - Invalid Condition, NEXT Subroutine, Row Reference, or Subscript Stop Expression
- invalid expression '%1'. Must be an {Li} or {Di} reference from this or a previous subscript
level, or an {iDj} reference from a previous subscript level.

Map: %2 - Data Access Variable Expression - invalid expression '%1'. Must be a {Li}, {Di},
or {iDj} reference from this or a previous subscript level.

Map: %2 - Map Data Retrieval Code - invalid expression '%1'. Must be a {Li}, {Di}, {iDj},
{%row}, {%rowraw}, or {*} (This field) reference.

Map: %2 - RowID Specifications - invalid expression '%1'. Must be a {Li} or any field from
Map Data.

Map: %2 - Subscript Expression - invalid expression '%1'. Must be a valid field reference. If this is the Master Map, it must be an IDKEY field.

Map: %2 - Map Data Field Name - invalid expression '%1'. Must be a valid field reference.

Map: %2 - Map Data Node - invalid expression '%1'. Must be a {Di} or {iDj} reference.

SQL does not support data type methods in languages other than COS in class %1 method %2.

DEFAULTDATA must be a listnode: %1

PARENT token used in storage but there is no parent relationship: %1

ID Property collation must be EXACT: %1

%2 parameter value must be a positive integer: %1.%2=%3

Incorrect numeric format in class %1 property %2 method %3

Foreign key '%1' cardinality does not match referenced key

BITSLICE index can only have one property: %1

A SUBVALUE index is defined but BuildValueArray method is not implemented: %1

Description

Studio was not able to parse the class definition for class '%1' correctly, possibly due to
non-matching {} or () characters, so we can not compile this class. Edit this in Studio and
correct the problem.

Can not save a read only method. This is because implementation is too large to put into property

An index must have at least one property: %1

A SUBVALUE index cannot be unique: %1

%2 parameter value must be an integer between 0 and 15: %1.%2=%3

Storage reference: '%1' used in '%2' is already registered for use by '%3'

Error registering reference '%1' for use by '%2': %3

Unable to recompile all classes in %SYS if system library database is read only

Class '%1' is in a database you do not have write permissions on so it can not be compiled

%Currency SCALE parameter value is final and cannot be overridden: %1.%2

SCALE parameter value cannot be negative: %1.%2

Class '%1' is in a database you do not have write permissions on so %2 cannot be defined
as a subextent.

Property '%1' in class '%2' is defined as 'not inheritable' but this is not supported.

Can not inherit relationship property '%1' in class '%2' as a secondary superclass.

Required constraint not supported on N-Cardinality relationship property '%1' in class '%2'.

Error reported while running generator for parameter '%1'

Index cannot reference a private property of a serial class: %1

No metadata created by '%1'.

Error calling metadata generator '%1'.

No method found for metadata generator '%1'.

Invalid codemode '%1' for metadata generator '%2'.

SQL Privilege Violation: '%1'

Error during Build or Purge Indices: $ZError = '%1'

Unable to grant all privileges on tables, views, and procedures to _PUBLIC for SAMPLES
namespace: $ZError = '%1'

SQL Map '%1', Data Field '%2', Node Value '%3' is invalid. Node Value is not allowed for index maps, only data maps.

Unable to grant SELECT privilege on tables Docbook.block to _PUBLIC for DOCBOOK
namespace: $ZError = '%1'

Unable to define default RowID Specifications for class %1, map %2, field %3. RowID Specifications must be defined manually for this map definition.

Invalid argument passed to %1. %2 parameter must be one or more of: %3.

Description

Invalid argument passed to %1. %2 parameter must be begin with one of: %3.

Invalid argument passed to %1. %2 parameter must be '%3'.

Invalid argument passed to %1. %2 parameter must be '9,ProcedureName'.

Failed to acquire lock on extent %1 in order to determine Map Block Counts for the extent

Invalid argument passed to %1. %2 parameter must be %3.

Unable to split global '%1' into segments for parallel work because '%2'.

Unable to split global '%1' into segments for parallel work.

Error during %SQLBuildPurgeIndexForRow: $ZError = '%1'

Feature is not supported for a Sharded table: '%1'.

Global name is missing for SQL map '%1' in table '%2'.

Sharded table's shard key (%1) must be the same as the idkey (%2) when the idkey (or identity field) is defined.

Sharded class '%1' must use storage type %Storage.Persistent, not storage type '%2'.

Sharded class '%1' must be ClassType 'persistent', not ClassType '%2'.

Sharded table's shard key (%1) must be the same as the idkey (%2) when the idkey is defined.

Sharded class '%1' must use storage type %Storage.Persistent, not storage type '%2'.

Sharded class '%1' must be ClassType 'persistent', not ClassType '%2'.

Table 1–14: General Error Codes - 5600 to 5799 (Macro Compiler Errors)

Description

Feature not supported for sharded class %1: %2 %3.

No class context: %1

Cannot resolve super class '%1'

Instance variable '%1' does not exist

Instance variable '%1' does not support array

Invalid class context for instance variable '%1'

Invalid usage of super - %1

Reference variable '%1' does not exist

Reference variable '%1' does not support array

Referenced macro not defined: '%1'

Function macro missing arguments: '%1'

Referenced macro missing right paren: '%1'

Too many arguments to macro: '%1'

Not enough arguments to macro: '%1'

Description

No closing %1 character inside '%2'

No open parenthesis after ##keyword

Invalid preprocessor ##keyword: ##%1

No closing parenthesis after ##%1

Invalid ##%1 argument '%1'

Need Table.Field for ##%1

No table '%1' for ##%1

No field '%1' in table '%1' for ##%1

Invalid argument '%1' to ##%1

No previous ##%1 (NEW%1) for ##%1(%1%1)

No macro name for #define

No closing paren for arglist

More than one macro parameter with #def1arg

Macro argument does not begin with %

Bad character in argument

##continue on last line

'%1' ignored; not preceded by #if or #ifdef

Null argument to '%1'

Error evaluating #if or #elseif argument (%1): %2

No macro name for #%1

No include file '%1'

No library file '%1'

No version #%1 for library file %1

Incorrect mode for #sqlcompile

##function on '%1' failed with an error: %2

#routine already specified for this macro source file

#routine cannot be specified after an sql statement

invalid routine name specified in #routine

cannot nest ##rtnref calls

invalid reference specified in ##rtnref

another element of the same name already exist

##expression on '%1' failed with an error: %2

Invalid macro name in #define or #def1arg: %1

##function use is restricted to embedded SQL

Description

Too many (%1) macros referenced on this line. This might indicate recursion in the macro definitions.

SPACE, TAB, "+", "-", "*", "/", "\", "|" characters not allowed in <marker> when using &SQL<marker>(...)<reverse-marker> syntax

Cannot do property

Cannot set method

Compiled class '%1' does not exist

Method '%1' does not exist

Parameter '%1' does not exist

Property '%1' does not exist

Method '%1' has no return value

Object instance required

Property '%1' required

Query '%1' does not exist

Collection property '%1' is required so must have at least one member

Relationship child/many property '%1' is required so must have at least one member

Statement type (%1) is not supported in #sqlcompile mode=deferred.

Attempt to reference instance variable '%1' in class method context.

Macro Preprocessor (MPP) Function '%1' failed with an error: %2.

Missing required name (macro compiler error)

Missing left paren (macro compiler error)

Missing right paren (macro compiler error)

No equal sign after set left (macro compiler error)

Unbalanced quotes (macro compiler error)

Unbalanced parentheses (macro compiler error)

Unbalanced #beginlit .. #endlit (macro compiler error)

Unexpected #else (macro compiler error)

Unexpected #elseif (macro compiler error)

Unexpected #endif (macro compiler error)

Unexpected end of line (macro compiler error)

Unexpected end of file (macro compiler error)

Incorrect delimiter (macro compiler error)

External package named %1 not supported (macro compiler error)

Macro nesting limit exceeded, check for circular macro reference (macro compiler error)

Description

No previous new for variable %1 (macro compiler error)

Embedded file '%1' not found (macro compiler error)

Compiling (macro compiler error)

Compile Complete! (macro compiler error)

Failed to file INT code (macro compiler error)

Failed to file MAC code (macro compiler error)

Module exceeded maximum PCODE size (macro compiler error)

Compile Failed! (macro compiler error)

Unable to split the code block, pcode is larger than %2 for routine '%1' (macro compiler error)

Unable to split the code for routine '%1' as it is not INT code (macro compiler error)

No current class context for #classcontext statement (macro compiler error)

Security violation opening object '%1'

Cannot access method '%1'

Class '%1' is abstract

Cannot instantiate abstract class '%1'

Cannot instantiate datatype class '%1'

Object '%1' is not registered

Procedure name: '%1' is not valid

Procedure: '%1' not found

Method not implemented: %1

Property is read only

Fail to instantiate object instance: %1

Fail to create new object instance: %1

Class '%1' is read only

Failed to create embedded object for '%1'

%DeleteExtent could not delete all instances of '%1'

Export was done on a system with a different locale: '%1'

Invalid table name: '%1'

Table already exists: '%1'

Class already exists: '%1'

Linking error: '%1'

Object open failed because '%1' key value of '%2' was not found

Object delete failed because '%1' key value of '%2' was not found

Description

Collection is read only

Cannot set Identity property unless IDENTITY_INSERT option is on: %1

Cannot update a previously assigned counter property value: %1:%2

Unable to get a lock on class inheritance structure '%1' within timeout.

Cannot acquire lock on referenced object for foreign key '%1' for '%2'

Cannot acquire lock on referenced object for referenced key '%1'

Instance of '%1' with '%2' key value = '%3' not found

Failed to lock extent for exclusive access: '%1'

Failed to lock extent for shared access: '%1'

Table 1–15: General Error Codes - 5800 to 5999

Description

Concurrency failure on update: object versions not the same for '%1'

Cannot set serial

Datatype validation failed on property '%1', with value equal to "%2"

Failed to acquire exclusive lock on instance of '%1'

Failed to acquire read lock on instance of '%1'

ID key not unique for extent '%1' : '%2' exists. Id counter location = '%3'

Lock type '%1' is invalid

Oref '%1' is invalid

Key not unique: %1

Object to Load not found, class '%1', ID '%2'

Object to Delete not found, class '%1', ID '%2'

Nothing to load, class '%1', ID '%2'

Null id, class '%1'

Null oid, class '%1'

Oid previously assigned, class '%1', ID '%2'

Too many calls to close

Transaction roll back failed

No properties selected in query: %1

Query is not closed

Too many arguments

Collection key '%1' is invalid

Cannot instantiate query: '%1'

Description

Formal argument invalid: '%1'

Cannot delete object, referenced by '%1'

Object referenced by '%1' does not exist

Not an instance of %1

Class '%1' does not support '%2' interface

Invalid cyclical dependency in save, class '%1'

Concurrency must be an integer from 0 to 4

Foreign Key constraint (%1) failed referential integrity check upon %2 in referencing extent

Foreign Key constraint (%1) failed upon %3 of object in %2 (referential action of %4)

Foreign Key constraint (%1) failed upon %3 of object in %2: At least 1 object exists which references key %4

At least one component of the ID value for class %1 is Null: '%2'

Value not an instance of property's type class: '%1::%2'

ID counter value is invalid, check the messages log: '%1'

You can not disconnect a collection that is already disconnected

Property type class '%3' is abstract: '%1::%2'

Null GUID: '%1'

You need %Development:use privilege to run this application.

Unable to add CSP item '%1' to project because it already includes '%2' which is same name but different case.

Unable to import file '%1' as this is not a supported type.

Unable to goto offset %1 in line %2 in file '%3' as line is not long enough.

Unable to goto line %1 in file '%2' as file too short.

Unable to instantiate user defined document '%1'.

User defined document '%1' not supported. No user defined document class in this namespace.

Item '%1' is not editable%2

To use Studio you must have %Developer:Use privilege.

You can not import the default project '%1'.

You can not export the default project '%1', rename project then export it.

Routine '%1' is of language type '%2' which is different to the language specified.

You can not add/remove '%1' to this project as it already contains the package '%2'.

Cannot modify library class

Cannot save library class

Invalid element type

Description

Invalid global reference %1

Invalid oid prefix

SQLBinding does not exist

Storage sql map data name required

Storage sql map name required

Storage sql map row IDSpec name required

Storage sql map subscript name required

Package routine prefix is too long

Package global prefix is too long

Another user has '%1' open for editing.

User '%2' in process '%3' has '%1' open for editing.

Item '%1' is not checked out of source control%2

Project does not have a Name

Invalid type for project item: '%1'

Name for project item is blank

No stream data to import

Unable to create source control class: %1

Project '%1' does not exist

Unable to create a new routine with name '%1'

Item '%1' is mapped from a database that you do not have write permission on.

The CSP/CSR page '%1' will be opened as Read Only because its source file is marked as
Read Only.

Can not save compiled dictionary classes.

Can not delete compiled dictionary classes.

Can not create new compiled dictionary classes.

Not logged into source control system so this action is unavailable

Routine name '%1' is too long

Unable to copy this project to a new name

Routine '%1' already exists and is of a different type to the current routine. Either rename your routine or delete the routine that already exists.

The file '%1' is invalid and terminates before a valid %RO file should, the routine '%2' may be truncated.

There are too many items in this file to return a list of the items correctly, the list of items is truncated.

Item '%1' is mapped from another namespace, so you can not save it here.

Description

Bad template mode '%1' can be one of TEMPLATE,ADDIN,NEW.

The source control class can not be changed from Studio, it is locked as '%1'.

Unable to decode this global format due to it being too long.

Unable to decode this global format is bad '%1'.

Package name supplied was '%1' but the real package name was '%2', case in inconsistent.

Rule family '%1' does not exist

Rule '%1' does not exist

Rule name is required

Attribute '%2' is required for tag '<%1>' on line number %3

The value of attribute %1, '%2', is invalid, on line number %3

Session ID is missing

Session ID '%1' does not exist

Failed to create class '%1': %2

There is no closing tag for the tag <%1> on line number %2

Must relogin with two factor protocol.

Character Set '%1' not installed, unable to perform character set translation

Page '%1' does not exist

HTTP response has an invalid Content-Type '%1'

CSP Application '%1' does not exist

Cannot allocate a license

Illegal CSP Request

HTTP method '%1' not supported by CSP

You are logged out, and can no longer perform that action

The action you are requesting is not valid

Must use CSP page '%1' from namespace '%2' and not current namespace '%3'

The CSP application '%1' must specify a namespace to run in

Timed out waiting for response

Redirected %1 times, appears to be a redirection loop

An error occurred and the specified error page could not be displayed - please inform the web master

<SCRIPT LANGUAGE=CACHE> tag is missing either RUNAT or METHOD attribute, on line number %1

Unable to redirect as HTTP headers have already been written and flushed

Unable to load page '%1' because its class name conflicts with the class '%2' that is already loaded

Description

Syntax error while parsing tag <%1> on line number %2

Syntax error while parsing CSP directive on line number %1

Include path type does not match filename specification on line number %1

Can only call this method/set this value in OnPreHTTP() before page has started to be displayed

Action not valid with this version of the Web Gateway on the web server

The CSP Server had an internal error: %1

The class '%1' referred to by the CSP:OBJECT tag '%2' on line %3 is not defined.

The name of the HTML form, '%1', is longer than 25 characters on line %2.

The HTML form '%1' is not bound to a valid csp object name on line %2.

The object variable '%1' to which form '%2' is bound on line %3 is not defined.

The tag name, '%1', is not unique in the form '%2' on line %3.

The CSPBIND attribute for SELECT with QUERY must be a persistent object reference on line %1.

CSP:OBJECT NAME attribute must be a valid identifier for tag '%1' on line %2.

Multiple CHECKBOX tags cannot be bound to a single value field on line %1.

%1 tag on line %2 has CSPBIND attribute, but is not in a bound form.

SCRIPT LANGUAGE=SQL tag cannot have both NAME and CURSOR attribute on line %1.

%1 attribute must be a valid identifier for %2 on line %3.

MODE attribute must be DISPLAY, LOGICAL, ODBC or SYSTEM on line %1.

Duplicate definition of SQL CURSOR '%1' on line %2.

SQL CURSOR '%1' is not defined and is used on line %2.

Duplicate definition of object '%1' on line %2.

Duplicate definition of the rule '%1'.

Class '%1' does not exist for rule '%2' on line %3.

The csp:search tag '%1' may have ONSELECT specified only with OPTION=POPUP on line %2.

The CSP rule version has changed - user rules need to be reloaded.

Query method did not return a value: %1.

Failed to lock CSP page.

CSPAppList query: invalid data in Fetch().

Directory '%1' for CSP Application '%2' does not exist

CSPPageLookup: Search error.

CSPPageLookup: CLASSNAME Missing.

CSPPageLookup: WHERE Missing.

Description

CSPPageLookup: Unable to create result set.

Unable to convert character set '%1'.

Unable to allocate new session.

Invalid SysLog level: %1.

Language changed by page directive on line number %1

Invalid language, '%1', specified on line number %2

Unknown charset, '%1', specified on line number %2

The CSP hyperevent request did not include a mandatory parameter so it can not be processed.

CSR:RULE LANGUAGE attribute value, %1, is invalid in rule %2.

Script tag language, '%1', does not match page language on line number %2

Static SQL tags are not supported on Basic pages on line number %1

An error occurred attempting to trade a CSP license for a named user license '%1'

Invalid format for SaveCallback for form %1

The CSP page '%1' is too large to load, we support pages up to 1.5Mb in size.

The persistent session is no longer available because the server process does not exist

Unable to lock session object as another process has this lock

Direction attribute is not 'forward' or 'backward' on line %1.

Direction part of WHERE, SELECT or ORDER attribute of csp:search must be ASC or DESC.

Value of cspSaveMsgEscape attribute must be None, HTML or JS on line number %1.

Session Id invalid.

Preserve=1 mode only supported with a real web server.

csp:include tag must include a PAGE attribute to specify the page to include.

Only the SELECT SQL command is allowed in SCRIPT LANGUAGE=SQL tag on line %1.

Page not found.

In order to run pages in this application we need an authenticated user.

Attempt to use a web session for service '%1' when session was started as service '%2'.

Current user is not authenticated to run service '%1'.

Methods that are defined in a CSP page must be classmethods on line %1.

The session is only using cookies for session management, but the browser provided a CSPCHD argument to this session.

System rules (name begins with %) and namespace local rules may not both be defined in the same file.

Session id '%1' not found.

Unable to create SOAP method %1

Description

You are not allowed to alter the SecurityContext property

CSP error trap called with no error information available.

The CSP application '%1' specifies a namespace '%2' that does not exist.

Unexpected attribute, %1, on line %2.

Unable to find CSP.ini CSP gateway file.

Unable to find CSP gateway username in CSP.ini file.

Unable to update the CSP.ini CSP gateway file.

Expecting second portion of two factor authentication to finish login process.

Table 1–16: General Error Codes - 6000 to 6199

Description

Entered Security Token '%1' did not match sent token.

Can not restore file '%1' because it contains OBJ routines

File '%1' is not a %RO output file

Unable to convert class format

Unable to export class as XML

Unable to import class from XML, details follow '%1'

The XML file does not contain a recognized import format

Unable to set 'Content-Length' header, since it's readonly.

Unable to set 'Connection' header.

Method not supported.

Already connected.

Need to be connected.

No response from POP server: %1.

Unable to make TCP/IP connection to mail server. An earlier connection may not have been closed.

TCP/IP session already terminated.

POP3 Server reported error: %1.

Invalid response to %1 command: %2.

Line read from mailbox should not be blank

TCP/IP session unexpected error: %1.

Description

Attempt to find location failed

Handler POP failed

Handler PUSH failed

Gateway failed: %1.

Query not Prepared.

Invalid %qacn.

Gateway: Invalid connection handle.

Gateway: Cannot allocate statement.

NamespaceList query: invalid data in Fetch().

Error in Macro Preprocessor: %1.

Timed out waiting for response.

'%1' property must be specified for SMTP.

Unable to open TCP/IP connection.

Unexpected initial message, server may not be SMTP server: %1.

Error response to SMTP %1: %2.

SMTP server connection failed during %1 command: %2.

Output charset must be specified on Unicode system.

Character > 255 not valid for quoted printable message

Nothing imported.

Failed to Initialize

RetType not VOID or HRESULT

RetType name not NULL

No class to compile: %1

Routine %1 object code not found

Database contains class definitions: %1

Cannot mount database: %1

Illegal Export Directory Name

Database doesn't exist: %1

Invalid identifier format

Invalid Statement Type: %1

Invalid Dynamic Query formal parameter %1

Invalid number of parameter values

Error Generating INTO clause:

Invalid conversion direction value

Description

Malformed serialized data

A valid %MessageDictionary is not specified by '%1'

No language specified.

Unable to find translate table for output charset: %1.

POP3 error: %1.

MessageNumber must be specified.

Unable to open TCP/IP socket to server %1

Somebody else is using the Monitor.

The Monitor is not running

The Monitor is already running

Memory allocation for the Monitor failed

Could not enable statistics collection for Monitor

Unable to open collection '%1'

Invalid extension type on compile '%1'

Problem rebuilding the class index

Unable to find default XML catalog file '%1'

Error loading global file '%1' : %2

SMTP Send failed for all specified email addresses.

Required argument missing

Invalid License Key Data

Could not open license key file '%1' for write.

Invalid value for ContentTransferEncoding: %1

%1 is not a block number.

Block %1 is not a bitmap block.

Can not compare routines '%1' and '%2' as they are different types

No implementation in source control class for action %1 with document %2

The class '%1' is not a valid Studio extension class.

Can not export '%1' type in %RO format for item '%2'.

XML exported abstract document data not formatted as CDATA.

License upgrade error: '%1'.

The operation is not licensed.

Unknown errors detected, but no error code reported

Unable to write to socket with SSL/TLS configuration '%1', error reported '%2'

If Content-Type is message/rfc822, the only part must be a %Net.MailMessage.

Description

Content-Transfer-Encoding for attached email must be '7bit' or '8bit'.

Invalid response from proxy '%1' on CONNECT command '%2'.

CONNECT command to proxy '%1' failed with response '%2'.

No boundary attribute specified for multipart Content-Type.

Unexpected boundary line found at beginning of MIME body.

Invalid MIME header format.

Unexpected end of message found. Invalid MIME format.

MIME message source must be defined using OpenFile or OpenStream.

HTTP header name too long to store '%1'.

Global name '%1' is not valid.

Error '%1' while using TCP/IP device $zu(189,1)='%2'

Unable to create temporary file for HTTP request

SSLConfiguration must be specified if UseSTARTTLS is true.

STARTTLS not supported for SMTP: %1.

Com Exception: '%1'

Com CoClass has no default Interface defined

Com CoClass default interface does not support automation

Unable to set new source control class as you do not have WRITE privileges needed update ^%SYS global

Item '%1' is marked as read only by source control hooks.

Unable to remove item '%1' from the project as is not present in the project.

Email must be retrieved with Fetch before GetAttachedEmail is called.

Connection to web server can not reuse existing socket as socket was closed to server '%1'.

Unable to verify SSL/TLS connected to correct system as no SSL certificate present for this socket.

No match between server name '%1' and SSL certificate values '%2'.

Unable to save CSP file '%1' because temp file '%2' was not created, check directory permissions.

Unable to use fulldeploy on class '%1' because it is a subclass of %SwizzleObject or
%XML.Adaptor.

Although Https property is enabled no SSLConfiguration is specified so unable to make an HTTPS connection to '%1'.

HTTP request redirected to HTTPS address but no SSLConfiguration is specified so unable to make an HTTPS connection to '%1'.

HTTP request redirected to server '%1' at location '%2' which then reported the embedded error.

Description

Unable to create HTTP Authorization header for %1 scheme.

Authentication error for %1 scheme.

External Interrupt request failed with an error: %1

Path associated with cookie '%1' is too long, maximum supported is 510 characters and it is '%2' characters for path '%3'.

Server does not support authentication.

Authentication expected but failed.

Table 1–17: General Error Codes - 6200 to 6399

Description

Cannot Create Object: %1

Cannot Create Message Handler: %1

Unexpected Element

SOAP message contains prohibited processing instruction

Element must be namespace qualified

Version Error, namespace must be %1.

Unexpected SOAPACTION value: %1

Unexpected Attribute

Wrong number of Attributes

Invalid Attribute value

Missing Attribute

Incorrect Attribute namespace

Attribute namespace not in scope

Attribute NOT qualified

Attribute value NOT qualified

Unsupported Transport

Add Operation Failed

Duplicate Element

Unknown Error

Internal Server Error

Mandatory Header NOT supported: %1

Invalid SoapBindingStyle keyword '%1' for WebMethod %2.

Invalid SoapBodyUse keyword '%1' for WebMethod %2.

Arguments to a Web Service may not be of type: %1.

A DTD cannot be generated for class: %1.

Description

Argument, %1, of WebMethod, %2, must be a simple type or SOAP enabled.

Server Application Error

Badly formed SOAP Message

XMLPROJECTION value is inconsistent with type of property: %1.

Invalid value for XMLPROJECTION of property: %1.

Invalid format for %XML.Adaptor: %1.

Datatype validation failed for tag, %1, with value: %2

XML input is not in proper format for tag: %1.

Required tag not present: %1

Unexpected namespace for tag: %1.

Referenced id not found, %1, for tag: %2.

Unexpected tag in XML input: %1.

Key attribute not specified for an array tag: %1.

Only one property may have XMLPROJECTION = content

SERVICENAME must be specified by overriding the SERVICENAME parameter.

The SOAP WebClient LOCATION parameter must specify http or https transport.

HTTP request to SOAP WebService returned unexpected status: %1.

HTTP request to SOAP WebService returned response with unexpected CONTENT-TYPE:
%1.

The location of the web service must be specified.

Client Web Method may not have an argument beginning with %: %1.

No response to SOAP request.

Unexpected encoding of SOAP response.

SOAP response is a SOAP fault: %1

A class referenced by an XMLENABLED class must be a subclass of %XML.Adaptor: %1

Collection property requires ELEMENTTYPE parameter in referenced class: %1

Cannot find message element '%1' in XML namespace '%2'

Datatype validation failed because no value found for tag, %1.

Datatype validation failed for tag %1. Unexpected tag <%2> found.

Tag expected, XML input, %1, is not in proper format as child of %2.

XML is not in proper format for DataSet record in field '%1', %2.

SubstitutionGroup for property '%1' is inconsistent with previous substitutionGroup.

XMLCHOICELIST for property '%1' may not contain literal type '%2'.

Invalid ENCODING parameter '%1' for property '%2'.

Description

XMLPROJECTION for property '%1' may not be ID unless this property is a persistent object.

Datatype validation failed for attribute, %1, with value %2 for element %3.

Unexpected value for XMLIGNORENULL class parameter: %1

Invalid value for XMLIO of property: %1.

Invalid value for XMLREFERENCE of property: %1.

Invalid value for XMLTYPECONSTRAINT of property: %1.

XMLREFERENCE and XMLTYPECONSTRAINT may be specified only for class references for property: %1.

XMLTYPECONSTRAINT may not be specified with XMLREFERENCE = ID for property:
%1.

XMLSUMMARY must a comma separated list of class properties.

Invalid value for XMLDEFAULTREFERENCE.

CLASS and QUERY must be specified for a typed dataset.

Duplicate WebMethod name not allowed: %1.

The QUERYNAME parameter and the classname (or XMLNAME override) may not be the same.

An %XML.DataSet cannot have the QueryName and DataSetName properties.

Cannot output a new XML document or change %XML.Writer properties until the current document is completed.

A root element must be written to contain child elements.

Type attribute, %1, does not specify valid type for XML input tag: %2.

XML output string is not available.

XML output string length is greater than the maximum string length.

An %XML.DataSet cannot be directly executed to get the query result.

%1 of class %2 must be able to differentiate child classes of %3.

Malformed SOAP Body in response.

Unexpected session cookie in session header.

Security header error: %1.

Cannot call EndDocument unless StartDocument called.

Root element, processing instruction or DOCTYPE may not be in root element.

Attribute may only be called immediately after Element or RootElement.

Invalid schema for %XML.Dataset at element '%1', %2.

Dataset schema does not match the specified typed %XML.Dataset: %1, %2 : %3 '= %4.

Dataset schema must be in XML input if %XML.Dataset is not typed.

Dataset name, row name and XML namespace must match XML schema for %XML.Dataset.

Description

%XML.Dataset may not have duplicated column name: %1.

Unable to load translate table '%1' for charset '%2'.

Cannot find message part in schema: %1

Internal error in XML Schema Wizard: %1

XML export cycle found in class: %1

Invalid value for XMLSTREAMMODE of property: %1.

XMLSTREAMMODE is not permitted for property %1, since it is not a character stream.

XMLNAME does not specify a valid XML name for property %1.

Invalid value for XMLFORMAT.

SAX XML Parser Error: %1

XML message file format invalid at Line %1 Offset %2.

Content Handler is NOT a subclass of %XML.SAX.ContentHandler

Unable to export item '%1' because XML export does not support items of this type. Will skip this item.

Unable to export item '%1' because can not instantiate user defined document type '%2'. Will skip this item.

CSP page '%1' does not have an associated application, skipping this item.

CSP file '%1' associated with page '%2' does not exist, skipping this item.

Item '%1' is invalid or does not have any data to export, skipping this item.

Class '%1' is in deployed mode and so can not be exported, skipping this item.

URL '%1' is malformed and cannot be processed

Schema definition for namespace '%1' does not exist.

Unable to find default namespace for class '%1'.

Schema moniker type '%2' (from schema '%1') is invalid.

SAX XML Parser Warning: %1

Errors reporting importing XML subelement in file '%1' at line '%2' offset '%3', skipping this item.

No subdocument to import, skipping this item.

Invalid value for XMLINHERITANCE: %1.

Property required in XML document: %1

Unexpected value for fixed attribute, %1, with value %2 for element %3.

Only CreateSequenceResponse response to WS-ReliableMessaging CreateSequence request is supported: %1.

Unsupported value of IncompleteSequenceBehavior in CreateSequenceResponse message:
%1.

Description

Unexpected WS-ReliableMessaging header: %1

WS-ReliableMessaging Sequence header expected but not present

WS-ReliableMessaging response Sequence header LastMessageNumber does not match request Sequence header

Only CloseSequenceResponse response to WS-ReliableMessaging CloseSequence request is supported: %1.

Only TerminateSequenceResponse response to WS-ReliableMessaging TerminateSequence request is supported: %1.

A %SOAP.RM.CreateSequence object may only be used once to call %StartRMSession in order to start an RM session.

SoapMessageName keyword may only be specified for a web service method: %1.

SoapAction keyword may only be specified for a web service method: %1.

Invalid value for HttpRequester: %1.

Unexpected attributes for element %1: %2

If a property is not of type string and has XMLPROJECTION = content, then all other properties must have XMLPROJECTION = attribute.

SOAP message has no body.

Invalid node type: %1.

Parent node may not be set directly.

Error scanning tree: element expected.

Binary SOAP protocol may not be used with %SOAP.WebRequest.

Unexpected class, %1, received for binary SOAP protocol. %2 expected.

Class must be XML enabled: %1.

Duplicate definition of XML schema %1 %2 for class %3.

Inconsistent use of encoded format for XML namespace %1.

Inconsistent definition of ELEMENTQUALIFIED or ATTRIBUTEQUALIFIED for classes in namespace %1.

Invalid format of SOAP binary %1.

Unexpected top logical block: %1.

Unexpected SOAP binary version number: %1.

Duplicate definition of class, %1, in SOAP binary message.

Object instance refers to unknown class with index %1 in SOAP binary message.

Duplicate specification of SOAPCLASSNAME for class %1.

ServiceName must be specified in SOAPCLASSNAME for class %1.

Multipart MIME SOAP message received with unexpected Content-Type header field: %1. Only SOAP with Attachments and MTOM are supported.

Description

The SOAPVERSION parameter specifies an unsupported SOAP version: %1.

SOAP version %1 is not supported for this web client.

SOAP encodingStyle %1 is not supported.

Mandatory Header NOT supported

Invalid value for SECURITYIN parameter: %1

WS-Security header is required.

Format of certificate file is invalid: %1.

Unsupported Encryption algorithm for WS-Security: %1.

Key encryption failed: %1.

Encryption failed: %1.

Invalid value for XMLMAPPING.

XMLPROJECTION as attribute or content not allowed for XMLMAPPING="sequence".

XMLPROJECTION must be "group" when referencing class with XMLMAPPING="sequence":
%1.

ARGUMENTSTYLE must be either "wrapped" or "message": %1.

Unexpected element in SOAP message: %1.

Unable to create security element: %1.

Signature validation failed: %1.

Invalid WS-SecureConversation DerivedKeyToken: %1.

Only RequestSecurityTokenResponseCollection response to WS-SecureConversation RequestSecurityToken request is supported: %1.

RequestSecurityTokenResponseCollection response to WS-SecureConversation RequestSecurityToken request with no elements is not supported.

Unexpected %1 in SecurityTokenRequestResponse: %2.

%1 element is not supported in SecurityTokenRequestResponse.

SecurityContextToken not returned in SecurityTokenRequestResponse.

Invalid SecurityContextToken in SecurityTokenRequestResponse: %1."

SecurityContextToken has no associated key.

Unexpected SecurityTokenRequestResponse to cancel request.

Table 1–18: General Error Codes - 6400 to 6599

Description

Element '%1', invalid attribute '%2'

Element '%1', attribute '%2' has invalid value %3

Element '%1' contains invalid attributes

Description

Element '%1', invalid

Element '%1' has invalid value '%2'

Specified namespace '%1' is invalid, MUST be '%2'

Cannot deduce Schema Type - No valid correspondence found

Cannot deduce Message Type - No valid correspondence found

Unsupported encoding '%1'

Element '%1', required attribute '%2' is missing

Element '%1' is missing

Element '%1' - cannot determine %2 for operation %3

Element '%1' - corresponding %2 %3

Element '%1' - duplicate name '%2'

Element '%1' - unsupported transport '%2'

Element '%1' - unrecognized %2 element '%3'

Element '%1' - message '%2' type or element attribute must be specified for a part.

Element '%1' - message '%2' parameters not found for literal encoding

Element '%1' - inconsistent %2 for operation %3

Element '%1' - parts list contains an undefined or multiply defined part name.

Element '%1' - %2 ParameterOrder parameter count mismatch

WSDL namespace is not defined for targetNamespace = %1.

SOAP namespace is not defined for targetNamespace = %1.

Element '%1' - message '%2' both type and element attribute may not be specified for a part.

Element '%1' - message '%2' Message Style must be used for document style message with 2 or more parts.

Both the Client and the Service class cannot be in the same package: %1.

Client class not defined: %1.

Cannot define configuration for client class, %1, since it is already configured by %2 which configures multiple classes.

The specified WSDL must have exactly one port which will supply the policy for %1.

Unexpected root element, %1, in %SOAP.Configuration XData block,%2.

Unexpected element, %1, in %3 XData block, %2.

Duplicate name of %SOAP.Configuration XData block: %1.

Duplicate configuration name, %1, for SOAP class, %2.

SOAP class name not specified for service in configuration: %1.

Method name not specified for method element in configuration: %1.

Description

Duplicate method name, %1, in configuration, %2.

Unexpected element, %1, from WS-Policy namespace in %SOAP.Configuration XData block, %2.

Name attribute of configuration, %1, does not match name of %SOAP.Configuration XData block, %2.

Invalid SOAP configuration class name %1.

Configuration not found, %1, in %SOAP.Configuration class, %2.

A policy assertion, %1, may not have text children in %SOAP.Configuration class, %2.

Internal error while analyzing policy in configuration %1: %2.

Unsupported assertion namespace "%1", assertion=%2, configuration=%3.

No supported policy alternative in configuration %1.

Policy assertion %1 is not supported in configuration %2.

Policy assertion %1 is not recognized in configuration %2.

Policy assertion %1 may not have wsp:Policy child element in configuration %2.

Policy assertion %1 has unsupported parameter %2 in configuration %3.

Policy assertion %1 Header parameter requires Namespace attribute in configuration %2.

Policy assertion %1 Header parameter requires Name attribute in configuration %2.

Policy assertion %1 does not support nested policy assertion %2 in configuration %3.

Policy assertion %1 requires a nested policy in configuration %2.

Policy assertion %1 is not in expected namespace in configuration %2.

Only one %1 may be specified in configuration %2.

No assertion parameters are supported for token %1 in configuration %2.

Unsupported token %1 in %2 assertion in configuration %3.

%1 requires AlgorithmSuite assertion in configuration %2.

Token %1 format error in configuration %2.

Unexpected value for sp:IncludeToken %1 in configuration %2.

%1 requires one %2 token in configuration %3.

%1 requires at least one token in configuration %2.

%1 element is not in expected namespace, in %3 XData block, %2.

Unexpected attribute %1 in %2 element in configuration %3.

Both cfg:FindField and cfg:FindValue must be specified if either is specified for sp:X509Token in configuration %1.

Unexpected value of cfg:FindField, %1, for sp:X509Token in configuration %2.

No local URI attribute for wsp:PolicyReference element in configuration %1.

Description

URI attribute, %1, for wsp:PolicyReference element does not reference a policy in configuration %2.

sp:Username token is not valid for a supporting token with sp:SignedParts or sp:EncryptedParts assertion.

cfg:wsdlElement="%1" does not specify a valid value for wsdlElement in configuration %2.

Method name not specified for method element in parameters XData in class: %1.

Method named %1 in method element of parameters does not exist in class %2.

Header element requires %1 attribute in class %2.

Conflicting DerivedKey assertions for %1 in configuration %2.

Action element requires non-empty value in class %1.

Only one SecurityContextToken may be specified for %1 in any alternative in configuration %2.

Value of cfg:Lifetime, %1, for sp:SecureConversationToken must be in hours as a floating point number in configuration %2.

Value of %1 parameter, %2, is not of expected type, %3, in configuration %4.

Unrecognized XSD type '%1'

Cannot determine corresponding class type for specified XSD type '%1'

Lock timeout. XML projection failed for class '%1' trying to lock '%2' with timeout '%3'

XML projection of an array of streams is not supported in property: %1.

SOAP session failure: new session created since session specified in session header was not found.

No schema definition was found in specified file.

Invalid value for XMLNILNOOBJECT, %1.

Cannot open QR Code %1 file %2.

Cannot open QR Code exception file %1.

QR Code file %1 not created.

Invalid correction level %1, must be one of 'L', 'M', 'Q', or 'H'.

QR Code not supported on this platform.

QR Code invalid Dimension parameter %1.

QR Code Correction Level %1 supports string length of %2, size of string is %3.

The QRCode string %1 may not have been correctly escaped. The partial QR Code is in the file %2.

Table 1–19: General Error Codes - 6600 to 6799

Description

BeanName is required.

Description

RootDir is required.

ClassPath is required.

App Server Home is required. (APPSERVERHOME is "".)

Java Home is required.

Path is required.

ServerType is required.

We only support QuickStatement interface.

%1 is neither Persistent nor a Session Bean. ClassList is %2. ClassList must contain only Persistent or Session Bean classes.

ClassList must be specified in projection or in calling this routine. ClassList must not be empty.

This server whose name is %1 is not defined in the EJB Wizard. Please pick one of WEBLOGIC, WEBLOGIC7, WEBLOGIC8, JBOSS, JBOSS3 or PRAMATI. JBOSS generates code for JBOSS 2.4.3 and 2.4.4 and JBOSS3 generates code for JBoss 3.X. For WebLogic
### 6.1 use WEBLOGIC, for WebLogic 7.0 use WEBLOGIC7, and for WEBLOGIC 8.1 use
WEBLOGIC8. Please read your release notes for the list of supported servers.

CMP generator for Class=%1 failed: CMP generation is only supported on classes with primary keys.

Common CPP output not set

getClassMethodsError: %1 className=%2

getClassPropertiesError: %1 className=%2

getClassQueriesError: %1 className=%2

getEJBClassNameError: %1 className=%2

getEJBClassNameError: %1

Common output not set

Common language generator object not set

EJB Easy projection is only supported on Windows. On UNIX® use EJB.

PersistenceType must be BMP or CMP.

CMP generation for Class=%1 failed. CMP generation can only be done if all required properties are CMP compatible. Property=%2 is not CMP compatible.

EJB generation for ClassList=%1 failed. ClassList must include at least one persistent class that is not a session bean.

WebLogic requires a testable to be defined to test connection existence in connection pooling.

Class %1 is not projectible. Projection is aborting. If the super of a class is not a
%RegisteredObject and all the methods of the super are server-only and the class has some methods that are not class methods then it is not projectible. If the super of a class is null then the class must contain only class methods.

Description

%1 is not a valid value for TRANSACTIONISOLATION valid values are TRANSACTION_READ_UNCOMMITTED, and TRANSACTION_READ_COMMITTED.

Class %1 is not projectible. Projection is aborting. For a class to be projectible all methods
in the class must have the same signature in the left-most super. There is a conflict on
Method %2.

Class %1 is not projectible. Projection is aborting. For a class to be projectible all properties
in the class must have the same declaration in the left-most super. There is a conflict on
Property %2.

Class %1 is not projectible. Projection is aborting. For a class to be projectible its left-most
super %2 must be a %Library.RegisteredObject or class %1 must be a "static" class: a class that has only class methods and no properties or instance methods.

Persistent Class %1 is not projectible. Projection is aborting. For a persistent class to be projectible its left-most super %2 must be a %Library.Persistent.

Class %1 is not up-to-date. Please recompile the class and try again.

Class %1 is not projectible to EJB. Projection is aborting. For a class to be projectible all its
child tables must be valid. Child table %2 is not valid.

getClientClassDefError: %1 className=%2

QueryGetInfoError: %1, className=%2, query=%3

Class %1 is a datatype and cannot be projected

It is not valid for a format flag to contain / (back-slash). The invalid format flag is %1.

The List that %1 is attempting to return on Class %2 is too big

getCountMethodsError: %1 className=%2

getCountPropertiesError: %1 className=%2

getCountQueriesError: %1 className=%2

Aborting EJB Projection of %1 because property %2 is required and yet is not supported by the EJB Wizard.

Class %1 is not exportable. It should extend %Compiler.LG.Exportable to be exportable.

%1: %2 className=%3

JAVAPACKAGE parameter conflicts with clientname parameter. JAVAPACKAGE is %1 and clientname parameter is %2

Server side code generation is not available. Please use cpp_generator for client side code generation.

Cannot generate code for class %1 because depends on class %2 which has a problem in its generation.

Cannot generate code for class %1 because depends on class %2 which cannot be generated for reason: %3.

Cannot generate code for class %1 because depends on class %2 which is serveronly class.

Skipping generation of class %1 because it depends on something that is not projectable and is not serveronly. Here is more information. %2

Description

Class %1 has an empty JavaBlock

Class %1 has more than one JavaBlock

Cannot project %1 because super %2 is collection.

Cannot project %1 as POJO because method %2 has ByRef argument.

Cannot project %1 because method %2 has abstract stream in return type or argument type.

Cannot project %1 because left-most super %2 is a stream.

Timed out waiting for lock on cache for class %1

QueryGetParamInfoEror: %1, className=%2, query=%3

Already Attached

Missing PID value

Invalid PID value

Target has exited debugger

Could not issue break to target

Error attaching to CSP server: %1

Not Attached

Error unattaching from target

Target not stopped

Could not attach to target

Invalid debugger target: %1

Unable to find mapping for breakpoint '%1'

Start target failed

Debugger Error: %1

Invalid PID value '%1'

Target already in debug mode

Target in signon mode

Table 1–20: General Error Codes - 6800 to 6999

Description

XSLT XML Transformer Error: %1

Error Handler is NOT a subclass of %XML.XSLT.ErrorHandler

Output Stream is NOT a subclass of %BinaryStream

Result Handler is NOT a subclass of %XML.XPATH.ResultHandler

Input Stream is NOT a subclass of %BinaryStream

%New() should NOT be called directly, use 'Create...' factory methods

Table 1–21: General Error Codes - 7000 to 7199

Description

TSQL compiler error: %1

TSQL: %1

ISQL compiler error: %1

ISQL: %1

TSQL querybuilder expected "%1" (got "%2")

You can't assign the result of a query to a variable

TSQL language mode requires procedureblock: '%1::%2'

Error opening class definition for "%1": "%2"

Unrecognized input: "%1"

Read: Missing "]"

Read: Missing quote at end of string

Read: Syntax error at or around "%1"

RunQuery: Procedure "%1" is not a query

RunQuery: Argument "%1" has already been passed (as "%2")

Specified Seek position (%1) is past end of file (%2)

FileStream Mode %1 does not include Read mode setting

FileStream Mode %1 does not include Write mode setting

No delegated input stream is bound to this MetaStream

No Translation Table mapping found for CharEncoding '%1'

IO Stream class %1 is not closeable

Delegated IO Stream class %1 is not Seekable

Object of type %1 is not a Stream object

Timed out after %2 seconds trying to open stream '%1'

Timed out after %2 seconds listening for an incoming connection on socket '%1'

Telnet Option %1 is not set

Error in telnet handshake; state=%1,current byte=%2

Timeout attempting telnet initialization handshake

Table 1–22: General Error Codes - 7200 to 7399

Description

Datatype value '%1' failed IsValidDT validation

Description

Datatype value '%1' length longer than MAXLEN allowed of %2

Datatype value '%1' length less than MINLEN allowed of %2

Datatype value '%1' greater than MAXVAL allowed of %2

Datatype value '%1' less than MINVAL allowed of %2

Datatype value '%1' not in VALUELIST '%2'

Datatype value '%1' is not a valid boolean

Datatype value '%1' is not a valid number

Datatype value '%1' is not a valid timestamp format

Datatype value '%1' does not match PATTERN '%2'

Datatype value '%1' contains invalid character/s '%2'

Datatype value '%1' is not a valid duration

Datatype value '%1' is not a valid uniqueidentifier/GUID format

Failed to open logfile %1 for output

Backup.General.ExternalFreeze cannot run with switch 10 or 13 already set

Failed to locate TCP information for all cluster members

Failed to switch journal files on other cluster members

Failed to switch journal file. Status = %1

Failed to switch local journal file. Status = %1

System failed to quiesce

Failed to place journal marker

Task %1 does not exist

Backup is currently running

Failed to open task %1

No backup recorded for task %1

Unknown platform in $zversion(1)

Error building list of log files: %1

Error building list of tasks: %1

Unable to determine base directory from: %1

Unable to create directory for storing the log file: %1

Unable to determine base directory for: %1

Unable to create backup output directory: %1

Failed to set up list of databases for backup

Unknown backup type: %1

Database %1 does not exist

Description

IRISTEMP cannot be included in a backup

Error building list of databases: %1

%1 is not part of the current backup list

Failed to start backup job

Failed to initialize IJC Device: %1

BACKUP^DBACK returned failure

Cannot modify a built-in system task

Invalid backup type: %1

Taskname not specified as argument to %New

Taskname must contain only alphanumeric characters

Task already exists

No backup volume specified

Unable to read file, Backup or Restore is in progress

Cannot open backup volume '%1'

This is not a %1 Backup File

Request to suspend write daemon cleared

Table 1–23: General Error Codes - 7400 to 7599

Description

TASKMGR is already running

Unable to open task (%1)

Selected User (%1) is not enabled

Task (%1) is not scheduled to run

Multiple times per day but DailyIncrement is 0

Operation requires %1 to change the task username

User does not exist (RunAsUser %1)

DailyEndTime must be after DailyStartTime

EndDate must be after StartDate

TimePeriodDay must be null or contain values 1 through 7 (%1) is not valid

Output Directory does not exist

Filename is not valid

Task Class is required but is null

Task Class (%1) does not exist in %2

Could not find task to delete

Description

Unknown Scheduling problem, New Time = Last Time

Failed to mark task as suspended (SQLCODE=%1)

Failed to mark task as resumed (SQLCODE=%1)

Invalid Suspend Flag (FLAG=%1)

Unable to send mail. The Mail Server is not defined.

Unable to send mail. No email addresses are defined.

Failed to update configuration for (%1) with error %2

Failed to send email (%1)

Unable to delete task, clear all run after references first

At least 1 Run day Monday - Sunday must be selected

Task repeating offset must be a positive integer

Invalid day of the month (%1)

Invalid weekly offset use 1 - 5

Invalid frequency time (DailyFrequencyTime) use 0 or 1

Run After Task value is required

Start Date and Time must be after the current date and time

Task job running

Task job untrapped error (%1)

Task job setup error (%1)

Task job timeout error

Task job post process error (%1)

Must enter a tape device.

Enter a valid number of days

SSH %3 Error '%1': %2

Table 1–24: General Error Codes - 7600 to 7799

Description

Invalid global format to import from

Unknown package format type: %1

Exported on version '%1' but this machine on version '%2' so unable to import

Delimited id's setting on exported version %1 on current system %2 so unable to import

Global node collision with class '%1'

Unable to deploy routine '%1' with no source (as specified by removesource parameter) as there is no OBJ code

Global node already in use '%1'

Description

Original data value at %1=%2 new value %3

New data has %1=%2 but in original global this does not exist

File '%1' does not contain an exported deployment

Signature on code '%1' incorrect so this item is not original released version

Invalid manifest specification '%1'

Invalid expression '%1': %2

Invalid special variable '%1'

Parser error parsing '%1' at offset %2: '%3'

Table 1–25: General Error Codes - 7800 to 7999

Description

Unable to start any worker jobs

Another job is modifying worker numbers

Worker job/s unexpectedly shut down

Job complete queue unexpectedly closed

Lock table full, aborting

Unable to create workers when we already have work being processed

Unable to signal all workers

Not all worker jobs started, was attempting to start '%1'

Can not count worker jobs because unable to get a lock

Unable to create workers in a worker process

Work queue API being called using an invalid queue name '%1' for $job='%2'

Unable to find classname for '%1' call

Work queue unexpectedly removed, shutting down.

Work queue received bad response from worker, shutting down.

Worker jobs not processing any work, so process appears to be stalled, shutting down.

Work queue not initialized, either due to a previous error causing a shutdown, or no call to Initialize before queuing work.

Class '%1' is classtype=system but this does not support any '%2' members

Classtype=system classes can only inherit from other classtype=system classes. Super class path '%1'.

Classtype='%1' is not a valid classtype. Valid values are '%2'.

Classtype=system classes do not support generator methods '%1'.

C++ method '%1' in class '%2' can not support alias methods associated with property '%3'.

C++ method '%1' in class '%2' returntype not supported '%3'.

Description

C++ method '%1' in class '%2' argument type not supported '%3'.

Service '%1' not enabled for application '%2'

Second Factor Login Failed for application '%1'

OnApplication callback aborted the application change and the login process

Second Factor cancelled by user

Can not call on class that is not subclass of %ZEN.Mobile.basePage and not mobile enabled '%1'

Invalid method to call on mobile page '%1'

Session event callback prevented session from being created.

The WebSocket Read operation has timed out

The Client has closed the WebSocket

Table 1–26: General Error Codes - 8000 to 8199

Description

Domain %1 already exists

Invalid SortField %1

Global name '%1' is reserved

Failed to process source

Failed to acquire lock on domain %1

External Id not found for internal id %1

Configuration %1 does not exist

Failed to initialize DirectInput

Failed to initialize Indexer

Failed to load KB %1

Failed to load LB %1

KB %1 Not Found

%1: Caught Error %2

Lister alias '%1' already in use by class %2 in this namespace

Failed to reset %1 from location %2

Nothing to process

Configuration with id %1 does not exist

Description

Domain name and id do not match

Domain %1 does not exist

Domain %1 is corrupt

Domain %1 must be opened in namespace %2

Domain with id %1 does not exist

Internal datasource error

Failed to delete source %1

Invalid Type: %1

Missing Source Id

Missing Source Field %1 in source %2

Missing Continuation Key in source %1

Missing Crc Field %1 in source %2

Missing Sentence Field %1 in source %2

Missing Relation Frequency in source %2

Missing %1 Attribute in source %2

Search string must be at least %1 characters long

Source not specified

Source does not exist (srcId: %1)

Source does not exist (extId: %1)

No metadata field specified

Metadata field does not exist (field ID: %1)

Metadata field does not exist (field name: %1)

Supplied metadata value '%1' not allowed for MD Domain %2

Metadata field '%1' already exists in this domain

Dictionary %1 does not exist

Dictionary item %1 does not exist

Dictionary term %1 does not exist

Dictionary element %1 does not exist

Dictionary item with URI '%1' already exists

Dictionary format class %1 does not exist

Internal error indexing dictionary terms

Metadata operator '%1' not supported for field %2

Metadata LOV %1 does not exist

Failed to convert buffer text to lower case

Description

Failed to create encoding object for %1

Failed to transcode string

Invalid indexer id specified

Unknown language id specified

Invalid object id specified

Failed to open file %1

Cannot index with no KB loaded

Indexer returned a data item larger than the maximum supported

Indexer failed while attempting to return output data

An invalid data processing object was passed to the indexer

Configuration %1 exists

Languages must be in $list format

Language %1 does not exist

Failed to load library: %1

Unable to instantiate iKnow Engine

Failed to lock iKnow Indexer

Can't open Lister id: %1

Ngram search is not enabled for this domain

Cannot overwrite an existing MD value in batch mode

Supplied metadata value count (%1) does not correspond to key count (%2)

Unable to open Converter id: %1

Unable to open Processor id: %1

There are still lists scheduled for a Batch load. Process them first or call Loader.Reset()

Indexer process failed

Failed to create Loader instance

Failed to split External ID: %1

Unknown Lister class or alias: %1

BuildExtIdFromName() should be called for an implementing Lister class, not the abstract one

Match ID does not exist: %1

Configuration ID (%1) and name (%2) do not match

Missing bitstring

Bad $List in %1

Failed to build: %1

Description

Failed to start any worker processes

Only a single Virtual Source ID is supported for this query

Virtual Source %1 not found in this domain

Dictionary element value is too long: %1

Group with id %1 does not exist

Source with external ID already exists: %1

String too long: %1

SkipList with name '%1' already exists

Dictionary with name '%1' already exists

SkipList with id %1 does not exist

Failed to load the requested iKnow language data. This may be due to an insufficiently large gmheap setting.

Cannot set domain parameter '%1' (either unknown or not user-configurable)

Domain must be empty (no sources or entities) before parameter '%1' can be changed.

iKnow is not available for this license

Illegal result parameter value: %1 (should be either empty for output var or start with ^ for global output)

STORE error while compiling query result. Either decrease page size or use global output.

Gateway Request Failed: %1

Gateway Request Exception: %1

Gateway Request No Data: %1

Gateway Request Timedout: %1

Table 1–27: General Error Codes - 8200 to 8299

Description

Maximum concept length must be positive.

User has no permissions to write to the database

Missing Lister Parameter at index %1 (%2)

Invalid Lister Parameter at index %1 (%2): "%3"

No Lister registered in this Loader instance. Use Loader.SetLister() first

There is no User Dictionary with name "%1

There is no User Dictionary with id %1

User Dictionary with name "%1" already exists

Invalid filter spec: "%1”

Missing Relation Dominance in source %1

Description

Missing Proximity Field %1 in source %2

%1 is not a valid Converter class

%1 is not a valid Processor class

Missing Concept Dominance in source %1

Missing %1 Field %2 in source %3

The supplied Matching Profile has not been saved since it was last modified

A Matching Profile named "%1" does not exist in domain %2

No sources can be added to a DeepSee-managed domain other than through DeepSee

A Matching Profile named "%1" already exists

"%1" is not a valid Matching Profile name (no colons allowed)

This feature is only supported for domains in versions %1 or above

This parameter has already been set to '%1' at the system level. Use UnsetSystemParameter() to clear it first

Failed to acquire lock

%1 object is modified, please save first.

Domain parameter '%1' can only be changed at the namespace level if no non-empty domains exist in this namespace.

Cannot change domain parameter '%1' to '%2'. Illegal value.

Metric with ID %1 does not exist

Metric '%1' does not exist

Metric target '%1' with ID %2 does not exist

Metric '%1' does not support target '%2'

Domain %1 is managed by %2 and cannot be updated directly.

%1 %2 is managed by %3 and cannot be updated directly.

Internal error in iKnow Engine: %1

At least one of the data structures required to resolve this query is not build or up-to-date. Please review your domain's build flags (%1).

Failed to load iKnow language model. File: %1, Line: %2, '%3'

The "system" domain only supports Virtual Sources

This operation is not supported in the "system" domain

The iKnow engine could not allocate enough memory to process the current document.

Stemming is not enabled for this domain.

Search string syntax error: brackets mismatch

Table 1–28: General Error Codes - 8300 to 8599

Description

No Pattern Argument

ICU Regular Expression Error Number %1 (0x%2)

Internal error in ICU regular expression library

Syntax error in regexp pattern

RegexMatcher in invalid state for requested operation

Unrecognized backslash escape sequence in pattern

Incorrect Unicode property

Use of regexp feature that is not yet implemented

Incorrectly nested parentheses in regexp pattern

Decimal number is too large for ICU library

Error in {min,max} interval

In {min,max}, max is less than min

Back-reference to a non-existent capture group

Invalid value for match mode flags

Look-Behind pattern matches must have a bounded maximum length

Regexps cannot have UnicodeSets containing strings

Octal character constant cannot be greater than 0377

Missing closing bracket on a bracket expression

In a character range [x-y], x is greater than y

Regular expression backtrack stack overflow

Maximum allowed match time exceeded

Matching operation aborted by user callback fn

Index, such as group number, is out of bounds

Illegal argument, such as empty string for Pattern

No file with GUID '%1'

No file with name '%1'

No file with name '%1' in '%2'

Multiple files with name '%1'

Multiple files with name '%1' in '%2'

No file prior to '%1'

No file prior to '%1' with prefix '%2'

Error: '%1' in Line %2 at offset %3 (%4)

Error: '%1' in Line %2 at offset %3 (%4)

Table 1–29: General Error Codes - 8600 to 8899

Description

Cannot return application license, session out of scope

Invalid licensed application name

Cannot open license key file '%1'.

Not a valid license key file.

License key has expired.

License key is invalid.

License key invalid for product version.

This system (%1 Cores) exceeds permitted CPU core limit (%2) for IRIS.

This system (%1 Cores) exceeds permitted CPU core limit (%2) for IRIS.

License key platform (%1) invalid for this platform (%2).

License keys are not accepted on a Single User (SU) platform.

Licensed Application (%1), Keyword (%2) value (%3) is not a number.

No license for Application (%1).

Application connection count overflowed for user.

XData Missing - class: %1 name: %2

Dispatch Map Schema Validation Failed

The notification is invalid

The notification protocol '%1' is not supported

The device token is invalid

The APNS connection to %1 timed out after %2s

The push notification attempt failed

The push notification attempt failed: %1

Invalid response from the APNS: %1

Connection to %1:%2 failed

The APNS terminated the connection

Connection to %1 failed

Unexpected HTTP status %1: '%2'

The push notification attempt failed

The push notification attempt failed: %1

Table 1–30: General Error Codes - 9000 to 9299

Description

Class (%1) is not up to date.

Name: (%1) already exists.

Class (%1) is generated.

Class (%1) cannot be opened.

Class (%1), method (%2) cannot be parsed.

Class (%1) is deployed.

Extent size must be a positive number '%1'.

Global name '%1' for '%2' is too long, must be no more than %3 characters in length. (See
Incompatibility History)

%1

"%1" is not a valid option.

"%1" is not a valid value for ImportTableExists option.

Datatype "%1" is not supported for import columns.

Table %1 already exists.

Cannot acquire lock on table name "%1" in namespace %2.

Syntax error in import table column specification.

Syntax error: do not use keyword "AS" with SQL datatype name "%1".

Datatype parameter "%1" is not supported.

Datatype parameter "%1" is not supported for datatype %2.

Cardinality "%1" is not supported.

Time format %1 is not supported.

Specified inverse relationship name "%1" conflicts with an existing property of class %2.

Must specify inverse relationship name for relationship property %1.

Option "%1" cannot be set at the connection level.

Must connect before creating MapReduceResult.

Must connect before executing command.

Must connect before executing command.

Cannot acquire lock needed to synchronize class %1.

Cannot acquire lock needed to rollback checkpoint for class %1.

No information available about synchronization job with ID %1.

Table 1–31: General Error Codes - 9300 to 9400

Description

Shard %1:%2:%3 returned error

%1 shards returned errors

Transaction with id "%1" has already been started

Invalid transaction state transition: %1 to %2

Invalid transaction state transition: %1 to %2

Cannot open ECP connection to host %1 port %2

Attempt to close ECP connection to host %1 port %2 failed

Failed to create semaphore %1

Cannot open TCP/IP connection to host %1 port %2

Failed to delete semaphore %1

Requested operation not supported for storage model %1

Parallel load of %1 is already in progress

%1 is not a sharded table

No compiled shard-local class found for %1

Class %1 has %2 storage definitions

Shard namespace %1 on host %2 is mapped to remote host %3

Cannot login user %1 to shard server at host %2, port %3

Timed out waiting for lock to update mappings

Connection to host %1 port %2 shard namespace %3 was reset, try operation again

Current namespace %1 has no shards configured

Current namespace %1 is not a shard

Instance is not current primary failover member

Namespace %1 has no shard %2

No app server is assigned to shard %1 with hostname %2 port %3 namespace %4

Invalid SQL statement

Error allocating Id range, SQLCODE %d, SQL message: %2

Cannot add shard while tables with user-defined shard keys exist

Could not activate new shard%1, must call ActivateNewShards method

Requested operation must be performed on host on which globals database of master namespace %1 resides

Cannot create shard routines database path %1, internal error %2

Cannot create shard routines database, .DAT database file already exists in %1

Cannot create shard routine namespace %1, a namespace with that name already exists

Description

Cannot create shard routines database %1, a database with that name already exists

No path defined for shard routines database %1

No shard routine namespace defined for master namespace %1

No shard routine namespace defined for master namespace %1

Drop table: %1.%DeleteExtent() failed, %2 rows not deleted

Authentication error: %1

%1 is not enabled on shard %2:%3:%4

Shard %1:%2:%3 does not allow incoming connections from IP address %4

Hostname %1 cannot be translated to an IP address

Message out of sequence, expected message code %1, got message code %2

IP address %1 specified in connect request does not match startup IP address %2

No hostname or IP address can be found for this system

Master namespace %1 on primary mirror failover member is not mirrored

Query shard cannot be mirrored

Cannot deassign data shard while sharded tables exist

Namespace %1 has no shard with hostname %2, port %3 namespace %4

Shard count %1 for entity %2 is greater than total shard count %3

Could not clean up deassigned shard %1:%2:%3

Shard %1:%2:%3 has already been assigned

Shard %1:%2:%3 has already been assigned to another master (%4:%5:%6))

Shard %1 in namespace %2 has no query shard %3

Namespace %1 has no shards configured

Shard %1 failed verification

%1 shards failed verification

Query shard %1 of shard %2 failed verification

%1 query shards failed verification

Internal error: unrecognized message code %1

MaxServers setting %1 too low, %2 needed

MaxServerConn setting %1 too low, %2 needed

MaxServers setting %1 and MaxServerConn setting %2 both too low, %3 needed

Query shard mapped to wrong directory %1, should be %2

Query shard mapped to wrong instance %1:%2, should be %3:%4

Shard assigned with backup member address %1:%2 is not actually mirrored

Description

Shard assigned with backup member address %1:%2 is actually configured with backup member address %3:%4

%1 is not a valid option

%1 is not a valid value for option %2

Master namespace %1:%2:%3 cannot be assigned as a shard

Globals database for mirrored shard %1:%2:%3 must be mirrored

Previous database for shard %1:%2:%3 must be dismounted before executing ReassignShard

Sharding is not enabled in this version

Timed out waiting for reply from job %1 on shard %2 %3:%4:%5

Cannot read valid reply from shard %1, port number %2 may be incorrect

Sharding service is not enabled on shard server

ECP service is not enabled

Instance requires restart because CPF file has been modified and not yet activated

Current instance of version %1 cannot access sharded cluster of version %2

Shard %1:%2:%3 version %4 cannot be assigned to sharded cluster of version %5

Instance has been upgraded, $SYSTEM.Sharding.Upgrade() must be run in master
namespace

### 1.9 Error Codes 15000 and Higher

Table 1–32: General Error Codes - 15000 and Higher

Description

15414

15511

15555

15808

16000

16001

16002

16003

16004

16005

16006

16007

The type of the index key property, '%3' in class '%1', index '%2', is not a serial type.

SQL Map '%1' is conditional but condition is null.

Incorrect string format in class %1, '%2' = %3.

$$$iFindIndexIdKeyError

Line: '%1' Offset: '%2'.

Couldn't open document '%1'.

Invalid JSON Content.

HTTP POST has no content.

Unknown request type '%1'.

Document '%1' does NOT exist.

Document '%1' name is invalid.

Couldn't parse debugger command '%1'.

Non-Numeric Error Codes

### 1.10 Non-Numeric Error Codes

Table 1–33: Miscellaneous Error Codes

Description

DisplayStringLoaderError

DisplayStringLoader error '%1'

DisplayStringLoaderException

DisplayStringLoader exception '%1'

DomainOrFilesEmpty

The Domain or Files parameter(s) must not be empty

ERROR

ERROR

ErrDisplayStringNotFound

DisplayString undefined for Id='%1', domain='%2', language='%3'

ErrNoSaveMasterStrings

Failed to save XData MasterStrings for MasterLanguage '%1' to file %2

MasterStringsNewer

MasterStringsOlder

%1 XData MasterStrings are newer than strings in %2 - first difference = %3

%1 Strings in %2 are newer than XData MasterStrings - first difference = %3

MessageDomainNotFound

The domain specified in the domain parameter was not found in any message file

NoStatusCode

(no error description)

OK

STATUS

OK

STATUS

UnknownStatusCode

Unknown status code:

WARNING

WARNING

XMLImportLocation

(ending at line %1 character %2)

The table below lists the SQL numeric error codes and their error messages for InterSystems IRIS® data platform. These codes are returned as the SQLCODE variable value.

Note: While this document lists error codes as negative values, JDBC and ODBC clients always receive positive values.

For example, if an ODBC or JDBC application returns error code 30, look up error code -30 in this table.

There are two SQLCODE values that do not represent an SQL error:

Table 2–1: SQL Error Codes 0 and 100

Description

Successful Completion

No (more) data

- SQLCODE=0 indicates successful completion of an SQL operation. For a SELECT statement, this usually means the
successful retrieval of data from a table. However, if the SELECT performs an aggregate operation, (for example:
SELECT SUM(myfield)) the aggregate operation is successful and an SQLCODE=0 is issued even when there is
no data in myfield ; in this case SUM returns NULL and %ROWCOUNT=1.

- SQLCODE=100 indicates that the SQL operation was successful, but found no data to act upon. This can occur for a
number of reasons. For a SELECT these include: the specified table contains no data; the table contains no data that
satisfies the query criteria; or ro w retrieval has reached the final ro w of the table. For an UPDATE or DELETE these
include: the specified table contains no data; or the table contains no ro w of data that satisfies the WHERE clause
criteria. In these cases %ROWCOUNT=0.

In Embedded SQL, when SQLCODE=100 the output host variables specified in the INT O clause are nulled.

### 2.2 SQLCODE -400

The SQLCODE -400 error “Fatal error occurred” is a general error. It is generated when a more specific SQLCODE error code is not available.

### 2.3 Retrieving SQL Message Texts

To determine the meaning of an SQLCODE numeric code, use the following ObjectScript statement:

ObjectScript

WRITE "SQLCODE=",$SYSTEM.SQL.Functions.SQLCODE(-nnn)

This SQLCODE() method can also be called as a stored procedure: %SYSTEM_SQL.Functions_SQLCODE(-nnn).

When possible (usually at SQL compile time), error messages include the name of the field, table, vie w, or other element that caused the error. Placeholders for these names are shown using the <name> syntax convention.

The %msg variable may contain an additional message error text for certain errors. For further details, refer to System
Variables.

The message texts returned are shown below in their English versions. The actual message text returned depends upon your locale setting.

For information on generating ObjectScript general errors from SQLCODE errors, see the %SYSTEM.Error class.

### 2.4 Table of SQL Error Codes and Messages

For ease of use, the SQL Error Codes Table has been divided into the following sub-tables:

- Error Codes 0 and 100

- Error Codes -1 to -99

- Error Codes -101 to -399

- Error Codes -400 to -500

- WinSock Error Codes -10050 to -11002 Table 2–2: SQL Error Codes -1 to -99

Description

-1

-2

-3

-4

-5

-6

-7

-8

-9

Invalid SQL statement

Exponent digits missing after 'E'

Closing quote (") missing

A term expected, beginning with one of the following: identifier, constant, aggregate, %ALPHAUP, %EXACT, %MVR, %SQLSTRING, %SQLUPPER, %STRING, %UPPER, $$, :, +, -, (, NOT, EXISTS, or FOR

Column number specified in ORDER does not match SELECT list

ORDER BY column after UNION not found as SELECT column

Exponent out of range

Invalid DATEPART code for DATEPART(), DATENAME(), DATEADD(), or DATEDIFF()

Incompatible SELECT lists used in UNION/INTERSECT/EXCEPT

Description

-10

-11

-12

-13

-14

-15

-16

-17

-18

-19

-20

-21

-22

-23

-24

-25

-26

-27

-28

-29

-30

-31

-32

-33

-34

-35

-36

-37

-38

-39

-40

-41

The SELECT list of the subquery must have exactly one item

A scalar expression expected, not a condition

A term expected, beginning with one of the following: identifier, constant, aggregate, $$, :, (, +, -, %ALPHAUP, %EXACT, %MVR, %SQLSTRING, %SQLUPPER, %STRING, or %UPPER

An expression other than a subquery expected here

A comparison operator is required here

A condition expected after NOT

Quantifier SOME expected after the FOR in the for-expression

A for-condition expected after the ( in the for-expression

IS (or IS NOT) NULL predicate can be applied only to a field

An aggregate function cannot be used in a WHERE or GROUP BY clause

Name conflict in the FROM list over label

Pointer->Field reference may not be modified by an INSERT or UPDATE statement

ORDER must specify column names, not numbers, when after 'SELECT *'

Label is not listed among the applicable tables

Ambiguous sort column

Input encountered after end of query

Missing FROM clause

Field is ambiguous among the applicable tables

Host variable name must begin with either % or a letter

Field not found in the applicable tables

Table or view not found

Field not (found/unique) in table(s)

Outer-join symbol ( =* or *= ) must be between two fields

No field(s) found for table

Optimizer failed to find a usable join order

INSERT/UPDATE/DELETE not allowed for non-updateable view

WITH CHECK OPTION (CHECKOPTION class parameter) not allowed for
non-updateable views

SQL Scalar/Aggregate/Unary function not supported for Stream fields

No master map for table

No RowID field for table

ODBC escape extension not supported

An extrinsic function call must have the form '$$tag^routine(...)'

Description

-42

-43

-44

-45

-46

-47

-48

-49

-50

-51

-52

-53

-54

-55

-56

-57

-58

-59

-60

-61

-62

-63

-64

-65

-66

-67

-68

-69

-70

-71

-72

-73

-74

-75

Closing quotes ("") missing following pattern match

Table is ambiguous within #IMPORT schema name list

Duplicate method or query characteristic

Duplicate method in ObjectScript query body

Required method missing in ObjectScript query body

Invalid method or query characteristic

Invalid trigger REFERENCING clause for the trigger's event

Trigger REFERENCING clause cannot be specified when trigger language not SQL

Trigger specifies UPDATE OF <fieldlist> clause when trigger language not SQL

SQL statement expected

Cursor (Already/Was Not) DECLAREd

Constant or variable expected as new value

Array designator (last subscript omitted) expected after VALUES

Invalid GRANT <role> TO or REVOKE <role> FROM

GRANT/REVOKE Action not applicable to an object of this type

Trigger specifies WHEN clause when trigger language not SQL

Duplicate field found in trigger UPDATE OF <fieldlist> clause

Cannot have more than one field

An action (%ALTER, SELECT, UPDATE, etc.) expected

Cursor not updateable

Additional new values expected for INSERT/UPDATE

Data exception - invalid escape character

Incompatible SELECT list is used in INSERT

Positive integer constant or variable expected

Redundant fields found in SELECT list

Implicit join (arrow syntax) not supported in ON clause

Legacy outer join (=*, *=) not supported in ON clause

SET <field> = <value expression> not allowed with WHERE CURRENT OF <cursor>

Multi-Line field only valid for LIKE, Contains ([), or NULL Comparison.

Multi-Line field must be the left operand of the Comparison.

Multi-Line field not valid in ORDER BY clause

Aggregates not supported in ORDER BY clause

Duplicate <select-list> alias names found

<trim_spec> and/or <trim_char> required before FROM in TRIM function.

Description

-76

-77

-78

-79

-80

-81

-82

-83

-84

-85

-86

-87

-88

-89

-90

-91

-92

-93

-94

-95

-96

-97

-98

-99

Cardinality mismatch between the SELECT-list and INTO-list.

Qualified column reference not allowed in this JOIN context.

Invalid transaction state.

Referencing key and referenced key must be the same size

Integer expected

Column Constraint expected

Multiple table %DESCRIPTION definitions found

Multiple table %FILE definitions found

Multiple table %NUMROWS definitions found

Multiple table %ROUTINE definitions found

Invalid field definition, no datatype defined

Invalid table name

Invalid field name

Invalid index name

Invalid view name

Transaction mode cannot be specified more than once

Level of isolation cannot be READ UNCOMMITTED or READ VERIFIED if READ WRITE specified

number of conditions for the DIAGNOSTICS SIZE must be exact numeric

Unsupported usage of OUTER JOIN

Operation disallowed by operation table

Specified level of isolation is not supported

Duplicate select-list names found.

License violation

Privilege violation

Table 2–3: SQL Error Codes -101 to -399

Description

-101

-102

-103

-104

-105

-106

Attempt to open a cursor that is already open

Operation (FETCH/CLOSE/UPDATE/DELETE/...) attempted on an unopened cursor

Positioned UPDATE or DELETE attempted, but the cursor is not positioned on any row

Field validation failed in INSERT, or value failed to convert in DisplayToLogical or
OdbcToLogical

Field validation failed in UPDATE

Row to DELETE not found

Description

-107

-108

-109

-110

-111

-112

-113

-114

-115

-116

-117

-118

-119

-120

-121

-122

-123

-124

-125

-126

-127

-128

-129

-130

-131

-132

-133

-134

-135

-136

Cannot UPDATE RowID or RowID based on fields

Required field missing; INSERT or UPDATE not allowed

Cannot find the row designated for UPDATE

Locking conflict in filing

Cannot INSERT into a 'Default Only' RowID or RowID based on field

Access violation

%THRESHOLD violation

One or more matching rows is locked by another user

Cannot INSERT/UPDATE/DELETE on a read only table

Cardinality mismatch on INSERT/UPDATE between values list and number of table columns.

Aggregates not supported in views

Unknown or non-unique User or Role

UNIQUE or PRIMARY KEY constraint failed uniqueness check upon INSERT

UNIQUE or PRIMARY KEY constraint failed uniqueness check upon UPDATE

FOREIGN KEY constraint failed referential check upon INSERT of row in referencing table

FOREIGN KEY constraint failed referential check upon UPDATE of row in referencing table

FOREIGN KEY constraint failed referential check upon UPDATE of row in referenced table

FOREIGN KEY constraint failed referential check upon DELETE of row in referenced table

UNIQUE or PRIMARY KEY Constraint failed uniqueness check upon creation of the constraint

REVOKE with RESTRICT failed.

FOREIGN KEY Constraint failed referential check upon creation of the constraint

Argument to scalar function %OBJECT() must be a stream field

Illegal value for SET OPTION locale property

Before Insert trigger failed

After Insert trigger failed

Before Update trigger failed

After Update trigger failed

Before Delete trigger failed

After Delete trigger failed

View's WITH CHECK OPTION validation failed in INSERT

Description

-137

-138

-139

-140

-141

-142

-143

-144

-146

-147

-148

-149

-150

-151

-152

-153

-154

-155

-159

-160

-161

-162

-163

-164

-165

-166

-178

-180

-181

-182

-183

-184

View's WITH CHECK OPTION validation failed in UPDATE

Cannot INSERT/UPDATE a value for a read only field

Concurrency failure on update: row versions not the same

Invalid length parameter passed to the SUBSTRING function

Invalid input value passed to the CONVERT function

Cardinality mismatch between the view-column-list and view query's SELECT clause

ORDER BY not valid in a view's query

A subquery is not allowed in an insert statement's set/values clause

Unable to convert date input to a valid logical date value

Unable to convert time input to a valid logical time value

CREATE VIEW, ALTER VIEW, or a view's query may not contain host variable
references

SQL Function encountered an error

Optimistic concurrency locking for a class definition failed

Index is not found within tables used by this statement

Index is ambiguous within tables used by this statement

SQL compile options comment contains invalid JSON string

Cannot UPDATE fields that are part of the shard key

INSERT/UPDATE into external (linked) cannot use an array variable for input of new
values

Specified field cannot be converted to columnar

Storage conversion failed

References to an SQL connection must constitute a whole subquery

SQL Connection is not defined

Heterogeneous queries via the JDBC gateway are not supported

Storage conversion completed successfully but the automatic cleanup of stale table data failed

Specified table does not have a cleanup to be done

Cleanup failed

Cannot apply partition range compiling embedded cached query

Model name not unique

Model or Trained Model not found

No query is defined for the model

Predicting Column cannot appear in the specified WITH column list

Provider class not found

Description

-185

-186

-187

-188

-189

-190

-191

-192

-193

-194

-201

-219

-220

-221

-222

-223

-225

-226

-227

-228

-229

-230

-231

-232

-233

-234

-235

-237

-241

-242

-250

-251

-252

Predicting Column only has one unique value in the dataset

Model's Provider is unavailable on this instance

ML Configuration not found

ML Configuration property is not supported for this provider

Cannot DROP the System Default ML Configuration

IntegratedML not permitted with current license

Model has no default trained model. It may not have been trained.

ML Configuration name not unique

Model Column / With Column type mismatch

Cannot specify NOT DEFAULT when the Trained Model name is the same as the
Model's DefaultTrainedModel

Table or view name not unique

External Language Server required by this query could not be started

Gateway query error

Gateway query GetConnection() failed

Gateway query AllocStatement() failed

Gateway query Prepare() failed

Gateway query BindParameters() failed

Gateway query Execute() failed

Gateway query Fetch() failed

Gateway query GetData() failed

Foreign table query error

Foreign tables Execute() failed

Foreign tables Fetch() failed

Foreign tables Close() failed

Cardinality mismatch between COLUMNS/VALUES clause and number of table columns

Invalid foreign server type

Invalid foreign data wrapper

Schema import for foreign table did not return column metadata

Parallel query queue error

Parallel query run-time error

Field in QUERY clause doesn't match provided external name

Sharded query queue error

Sharded query run-time error

Description

-253

-300

-301

-302

-303

-304

-305

-306

-307

-308

-309

-310

-311

-312

-313

-314

-315

-316

-317

-319

-320

-321

-322

-324

-325

-326

-327

-328

-329

-333

-334

-340

Sharded INSERT/UPDATE/DELETE run-time error

DDL not allowed on this table definition

No Savepoint name

Savepoint names starting with "SYS" are reserved

No implicit conversion of Stream value to non-Stream field in UPDATE assignment is supported

Attempt to add a NOT NULL field with no default value to a table which contains data

Attempt to make field required when the table has one or more rows where the column value is NULL

Column with this name already exists

Primary key already defined for this table

Identity column already defined for this table

The left operand of %CONTAINS is not a property that supports the %Text interface

Foreign key references non-existent table

Foreign key with same name already defined for this table

Invalid schema name. Must use delimited identifiers to reference this schema name

Condition expression not supported for Stream fields

Foreign key references non-unique key/column collection

Constraint or Key not found

Foreign key references non-existent key/column collection

Cannot DROP Constraint - One or more Foreign Key constraints reference this Unique constraint

Referenced table has no primary key defined

Cannot DROP table - One or more Foreign Key constraints reference this table

Cannot DROP view - One or more views reference this view

Cannot DROP column — column is defined on one or more indexes or constraints

Index with this name already defined for this table

Index cannot be dropped because it is the IDKEY index and the table has data

Duplicate TUNE TABLE option clause found

Duplicate table option found

Duplicate foreign server option found

Required foreign server option missing

No such index defined

Index name is ambiguous. Index found in multiple tables.

No such database (namespace) defined

Description

-341

-342

-343

-344

-350

-356

-357

-358

-359

-360

-361

-362

-363

-364

-365

-366

-370

-371

-372

-373

-374

-375

-376

-377

-378

-380

-381

-382

-383

-385

-386

Database file already exists

Cannot delete system namespace

Invalid database name

Cannot drop database that you are currently using or connected to

An unexpected error occurred executing SqlComputeCode

SQL Function (function stored procedure) is not defined to return a value

SQL Function (function stored procedure) is not defined as a function procedure

SQL Function (function stored procedure) name not unique

SQL Function (function stored procedure) not found

Class not found

Method or Query name not unique

Method or Query not found

Trigger not found

Trigger with same EVENT, TIME, and ORDER already defined

Trigger name not unique

Schema name mismatch between trigger name and table name

SQL CALL, more arguments specified than defined in the stored procedure

:HVar = CALL ... Specified for a procedure which does not return a value

Support for extrinsic function calls are disabled

An extrinsic function call may not call a % routine

Cannot alter the datatype of a field to/from a stream type when the table contains data

Cannot ROLLBACK to unestablished savepoint

Unsupported CAST target specified

Field appears more than once in assignment list of insert or update statement

Datatype mismatch, explicit CAST is required

Invalid or Missing argument to scalar function

Too many arguments to scalar function

CTE name defined more than once

CTE used for any statement type other than select

CTE statements over xDBC are only supported by newer versions of the xDBC driver

Cannot use more than one of TOP, LIMIT or ANSI styles

Table 2–4: SQL Error Codes -400 to -500

Description

-400

-401

-402

-405

-406

-407

-408

-409

-410

-411

-412

-413

-415

-416

-417

-422

-425

-426

-427

-428

-429

-430

-431

-432

-450

-451

-452

-453

-454

-456

-459

-460

Fatal error occurred

Fatal Connection error

Invalid Username/Password

Unable to read from communication device

Unable to Write to Server

Unable to Write to Server Master

Unable to start server

Invalid server function

Invalid Directory

No stream object defined for field

General stream error

Incompatible client/server protocol

Fatal error occurred within the SQL filer

Info Error

Security Error

SELECT request processed via ODBC, JDBC, or Dynamic SQL cannot contain an
INTO clause

Error processing stored procedure request

Error preparing stored procedure

Invalid stored procedure name

Stored procedure not found

Invalid number of input/output parameters for stored procedure

Cannot initialize procedure context

Stored procedure parameter type mismatch

Function returned multiple rows when only a single value is expected

Request timed out due to user timeout

Unable to receive server message

Message sequencing error

Error in user initialization code

Error sending external interrupt request

SQL query execution interrupted by user

Kerberos authentication failure

General error

Description

-461

-462

-463

-464

-465

-466

-467

-468

-469

-470

-471

-472

-473

-474

-475

-476

-481

-482

-483

-478

-500

Communication link failure

Memory allocation failure

Invalid column number

Function sequence error

Invalid string or buffer length

Invalid parameter number

Column type out of range

Fetch type out of range

Driver not capable

Option value changed

Duplicate cursor name

A collection-valued property was expected

Schema not found

Explain does not support the following SQL statement type: INSERT

Schema is not empty

Schema already exists

Explain does not support the following SQL statement type: INSERT

Schema is not empty

Comparison between DATE and TIMESTAMP values of 'dStart' and 'CURRENT_TIMESTAMP' detected where ANSI SQL standard output may differ from legacy IRIS SQL output. Use CAST to ensure same-datatype comparisons. System-wide flag is set to raise this as an error.

Query recompiled: Result Set mismatch

Fetch row count limit reached

Table 2–5: WinSock Error Codes -10050 to -11002

Description

-10050

-10051

-10052

-10054

-10055

-10056

-10057

-10058

WinSock: Network is down

WinSock: Network is unreachable

WinSock: Net dropped connection or reset

WinSock: Connection reset by peer (due to timeout or reboot)

WinSock: No buffer space available

WinSock: Socket is already connected

WinSock: Socket is not connected

WinSock: Cannot send after socket shutdown

Description

-10060

-10061

-10064

-10065

-10070

-10091

-10092

-10093

-11001

-11002

WinSock: Connection timed out

WinSock: Connection refused

WinSock: Host is down

WinSock: No route to host

WinSock: Stale NFS file handle

WinSock: Network subsystem is unavailable

WinSock: WINSOCK DLL version out of range

WinSock: Successful WSASTARTUP not yet performed

WinSock: Host not found

WinSock: Nonauthoritative host not found

This section contains the TSQL error messages for InterSystems IRIS® data platform. To use these messages, your application’s TSQL procedure must reference the master..sysmessages system table. InterSystems IRIS does not support all TSQL features mentioned in these messages.

Table 3–1:TSQL Error Codes - 0 to 99

Description

Version date of last upgrade: 10/11/90

Warning: Fatal error %d occurred at %S_DATE. Note the error and time, and contact your system administrator.

Table 3–2:TSQL Error Codes - 100 to 199

Description

Incorrect syntax near '%.*ls'.

The %S_MSG that starts with '%.*ls' is too long. Maximum length is %d.

ORDER BY items must appear in the select list if the statement contains a UNION operator.

Unclosed quotation mark before the character string '%.*ls'.

Too many table names in the query. The maximum allowable is %d.

The column prefix '%.*ls' does not match with a table name or alias name used in the query.

The ORDER BY position number %ld is out of range of the number of items in the select list.

There are more columns in the INSERT statement than values specified in the VALUES clause. The number of values in the VALUES clause must match the number of columns specified in the INSERT statement.

There are fewer columns in the INSERT statement than values specified in the VALUES clause. The number of values in the VALUES clause must match the number of columns specified in the INSERT statement.

'%ls' must be the first statement in a query batch.

Description

Variables are not allowed in the %ls statement.

Missing end comment mark '*/'.

Browse mode is invalid for a statement that assigns values to a variable.

Only one expression can be specified in the select list when the subquery is not introduced with EXISTS.

The %S_MSG name '%.*ls' contains more than the maximum number of prefixes. The maximum is %d.

Only members of the sysadmin role can specify the %ls option for the %ls statement.

Must pass parameter number %d and subsequent parameters as '@name = value'. After the form '@name = value' has been used, all subsequent parameters must be passed in the form '@name = value'.

The select list for the INSERT statement contains fewer items than the insert list. The number of SELECT values must match the number of INSERT columns.

The select list for the INSERT statement contains more items than the insert list. The number of SELECT values must match the number of INSERT columns.

The %ls option is allowed only with %ls syntax.

Batch/procedure exceeds maximum length of %d characters.

CREATE PROCEDURE contains no statements.

Case expressions may only be nested to level %d.

The name '%.*ls' is not permitted in this context. Only constants, expressions, or variables allowed here. Column names are not permitted.

Fillfactor %d is not a valid percentage; fillfactor must be between 1 and 100.

Cannot perform an aggregate function on an expression containing an aggregate or a subquery.

The size (%d) given to the %S_MSG '%.*ls' exceeds the maximum allowed for any data type (%d).

The label '%.*ls' has already been declared. Label names must be unique within a query batch or stored procedure.

A GOTO statement references the label '%.*ls' but the label has not been declared.

The variable name '%.*ls' has already been declared. Variable names must be unique within a query batch or stored procedure.

Cannot use a BREAK statement outside the scope of a WHILE statement.

Cannot use a CONTINUE statement outside the scope of a WHILE statement.

Must declare the variable '%.*ls'.

Correlation clause in a subquery not permitted.

Cannot assign a default value to a local variable.

Can only use IF UPDATE within a CREATE TRIGGER statement.

Description

A SELECT statement that assigns a value to a variable must not be combined with data-retrieval operations.

Incorrect syntax for definition of the '%ls' constraint.

A COMPUTE BY item was not found in the order by list. All expressions in the compute by list must also be present in the order by list.

Cannot use an aggregate or a subquery in an expression used for the group by list of a GROUP BY clause.

ORDER BY items must appear in the select list if SELECT DISTINCT is specified.

Could not allocate ancillary table for a subquery. Maximum number of tables in a query (%d) exceeded.

An aggregate may not appear in the WHERE clause unless it is in a subquery contained in a HAVING clause or a select list, and the column being aggregated is an outer reference.

Incorrect time syntax in time string '%.*ls' used with WAITFOR.

Time value '%.*ls' used with WAITFOR is not a valid value. Check date/time syntax.

Both terms of an outer join must contain columns.

'%.*ls' is an invalid money value.

Invalid usage of the option %.*ls in the %ls statement.

%S_MSG is not allowed in %S_MSG.

'%.*ls' is not a recognized %ls option.

Incorrect syntax near the keyword '%.*ls'.

An aggregate may not appear in the set list of an UPDATE statement.

For DROP INDEX, you must give both the table and the index name, in the form tablename.indexname.

Rule does not contain a variable.

Rule contains more than one variable.

The compute by list does not match the order by list.

GROUP BY expressions must refer to column names that appear in the select list.

Privilege %ls may not be granted or revoked.

'%ls' does not allow specifying the database name as a prefix to the object name.

Cannot create a trigger on a temporary object.

The %S_MSG '%.*ls' is out of the range of computer representation (%d bytes).

A column has been specified more than once in the order by list. Columns in the order by list must be unique.

Line %d: Incorrect syntax near '%.*ls'.

Description

Cannot use SELECT INTO in browse mode.

Cannot use HOLDLOCK in browse mode.

The definition for column '%.*ls' must include a data type.

The %ls function requires %d arguments.

The IDENTITY function can only be used when the SELECT statement has an INTO clause.

A RETURN statement with a return value cannot be used in this context.

Cannot use the OUTPUT option when passing a constant to a stored procedure.

There are too many parameters in this %ls statement. The maximum number is %d.

Cannot use the OUTPUT option in a DECLARE statement.

Table and column names must be supplied for the READTEXT or WRITETEXT utility.

The scale (%d) for column '%.*ls' must be within the range %d to %d.

Data stream is invalid for WRITETEXT statement in bulk form.

Data stream missing from WRITETEXT statement.

Cannot specify a log device in a CREATE DATABASE statement without also specifying at least one non-log device.

The %ls function requires %d to %d arguments.

Some part of your SQL statement is nested too deeply. Rewrite the query or break it up into smaller queries.

The scale must be less than or equal to the precision.

The object or column name starting with '%.*ls' is too long. The maximum length is %d characters.

A SELECT INTO statement cannot contain a SELECT statement that assigns values to a variable.

'%.*ls' is not a recognized %S_MSG.

SELECT INTO must be the first query in an SQL statement containing a UNION
operator.

EXECUTE cannot be used as a source when inserting into a table variable.

Browse mode is invalid for statements containing a UNION operator.

An INSERT statement cannot contain a SELECT statement that assigns values to a variable.

Table 3–3:TSQL Error Codes - 200 to 299

Description

Procedure '%.*ls' expects parameter '%.*ls', which was not supplied.

Description

Invalid type '%s' for WAITFOR. Supported data types are CHAR/VARCHAR, NCHAR/NVARCHAR, and DATETIME. WAITFOR DELAY supports the INT and SMALLINT data types.

The name '%.*ls' is not a valid identifier.

Normalization error in node %ls.

All queries in an SQL statement containing a UNION operator must have an equal number of expressions in their target lists.

Operand type clash: %ls is incompatible with %ls

Invalid column name '%.*ls'.

Invalid object name '%.*ls'.

Ambiguous column name '%.*ls'.

Syntax error converting datetime from binary/varbinary string.

Expression result length exceeds the maximum. %d max, %d found.

Insert Error: Column name or number of supplied values does not match table definition.

Procedure expects parameter '%ls' of type '%ls'.

Maximum stored procedure, function, trigger, or view nesting level exceeded (limit %d).

Arithmetic overflow error for data type %ls, value = %ld.

FIPS Warning: Implicit conversion from %ls to %ls.

Object ID %ld specified as a default for table ID %ld, column ID %d is missing or not of type default.

Object ID %ld specified as a rule for table ID %ld, column ID %d is missing or not of type default.

%ls statement not allowed within multi-statement transaction.

%ls permission denied on object '%.*ls', database '%.*ls', owner '%.*ls'.

%ls permission denied on column '%.*ls' of object '%.*ls', database '%.*ls', owner '%.*ls'.

No such default. ID = %ld, database ID = %d.

Arithmetic overflow error for type %ls, value = %f.

The column '%.*ls' in table '%.*ls' cannot be null.

There is insufficient result space to convert a money value to %ls.

Cannot convert a char value to money. The char value has incorrect syntax.

The conversion from char data type to money resulted in a money overflow error.

There is insufficient result space to convert a money value to %ls.

There is insufficient result space to convert the %ls value (= %d) to the money data type.

Description

Syntax error converting datetime from character string.

The conversion of a char data type to a datetime data type resulted in an out-of-range datetime value.

Type %.*ls is not a defined system type.

The conversion of the %ls value '%.*ls' overflowed an %hs column. Use a larger integer column.

Syntax error converting the %ls value '%.*ls' to a column of data type %ls.

The conversion of the %ls value '%.*ls' overflowed an int column. Maximum integer value exceeded.

Could not allocate ancillary table for query optimization. Maximum number of tables in a query (%d) exceeded.

The data type %ls is invalid for the %ls function. Allowed types are: char/varchar, nchar/nvarchar, and binary/varbinary.

Implicit conversion from data type %ls to %ls is not allowed. Use the CONVERT function to run this query.

Ad hoc updates to system catalogs are not enabled. The system administrator must reconfigure SQL Server to allow this.

Disallowed implicit conversion from data type %ls to data type %ls, table '%.*ls', column '%.*ls'. Use the CONVERT function to run this query.

'%.*ls' is not a recognized function.

%ls permission denied in database '%.*ls'.

Must specify table to select from.

Column name '%.*ls' appears more than once in the result column list.

Transaction count after EXECUTE indicates that a COMMIT or ROLLBACK TRANSACTION statement is missing. Previous count = %ld, current count = %ld.

Object '%.*ls' cannot be found.

Cannot run SELECT INTO in this database. The database owner must run sp_dboption to enable this option.

Object '%.*ls' cannot be modified.

Column '%.*ls' cannot be modified because it is a computed column.

Cannot update a timestamp column.

Cannot insert a non-null value into a timestamp column. Use INSERT with a column list or with a default of NULL for the timestamp column.

The text, ntext, and image data types cannot be used in a GROUP BY clause.

The text, ntext, and image data types are invalid in this subquery or aggregate expression.

Only text, ntext, and image columns are valid with the TEXTPTR function.

%d is not a valid style number when converting from %ls to a character string.

Description

The '%.*ls' procedure attempted to return a status of NULL, which is not allowed. A status of 0 will be returned instead.

READTEXT cannot be used on inserted or deleted tables within an INSTEAD OF trigger.

Rules cannot be bound to text, ntext, or image data types.

The READTEXT, WRITETEXT, and UPDATETEXT statements cannot be used with views or functions.

The logical tables INSERTED and DELETED cannot be updated.

The %ls statement is not allowed within a trigger.

The PATINDEX function operates on char, nchar, varchar, nvarchar, text, and ntext data types only.

CAST or CONVERT: invalid attributes specified for type '%.*ls'

There is insufficient result space to convert a smallmoney value to %ls.

Cannot convert char value to smallmoney. The char value has incorrect syntax.

The conversion from char data type to smallmoney data type resulted in a smallmoney overflow error.

Syntax error converting character string to smalldatetime data type.

The conversion of char data type to smalldatetime data type resulted in an out-of-range smalldatetime value.

The conversion from datetime data type to smalldatetime data type resulted in a smalldatetime overflow error.

The DATEADD function was called with bad type %ls.

Table 3–4:TSQL Error Codes - 300 to 399

Description

Query contains an outer-join request that is not permitted.

The table '%.*ls' is an inner member of an outer-join clause. This is not allowed if the table also participates in a regular join clause.

The text, ntext, and image data types cannot be compared or sorted, except when using IS NULL or LIKE operator.

Index ID %d on table '%.*ls' (specified in the FROM clause) does not exist.

Index '%.*ls' on table '%.*ls' (specified in the FROM clause) does not exist.

Cannot use text, ntext, or image columns in the 'inserted' and 'deleted' tables.

Cannot reference text, ntext, or image columns in a filter stored procedure.

An insufficient number of arguments were supplied for the procedure or function %.*ls.

Cannot use GROUP BY ALL with the special tables INSERTED or DELETED.

Table 3–5:TSQL Error Codes - 400 to 499

Description

Unimplemented statement or expression %ls.

Invalid operator for data type. Operator equals %ls, type equals %ls.

The %ls operation cannot take a %ls data type as an argument.

COMPUTE clause #%d 'BY' expression #%d is not in the order by list.

COMPUTE clause #%d, aggregate expression #%d is not in the select list.

The text, ntext, and image data types cannot be used in an ORDER BY clause.

Data type %ls of receiving variable is not equal to the data type %ls of column '%.*ls'.

The length %d of the receiving variable is less than the length %d of the column '%.*ls'.

Could not load sysprocedures entries for constraint ID %d in database ID %d.

Could not find row in sysconstraints for constraint ID %d in database ID %d.

Could not find new constraint ID %d in sysconstraints, database ID %d, at compile time.

Could not resolve table name for object ID %d, database ID %d, when compiling foreign key.

Could not bind foreign key constraint. Too many tables involved in the query.

Could not find CHECK constraint for '%.*ls', although the table is flagged as having one.

Could not open referenced table ID %d in database ID %d.

Could not resolve the referenced column name in table ID %d.

Could not resolve the referencing column name in table ID %d.

Could not find FOREIGN KEY constraints for table '%.*ls' in database ID %d although the table is flagged as having them.

Cannot use the '%ls' function on a remote data source.

Invalid use of '%s' within a function.

Select statements included within a function cannot return data to a client.

COLLATE clause cannot be used on expressions containing a COLLATE clause.

Cannot resolve collation conflict for %ls operation.

Expression type %ls is invalid for COLLATE clause.

Invalid collation '%.*ls'.

Collation conflict caused by collate clauses with different collation '%.*ls' and '%.*ls'.

Code page translations are not supported for the text data type. From: %d To:
%d.

Description

Cannot resolve collation conflict for column %d in %ls statement.

COLLATE clause cannot be used on user-defined data types.

Collation '%.*ls' is supported on Unicode data types only and cannot be set at the database or server level.

The last statement included within a function must be a return statement.

Implicit conversion of %ls value to %ls cannot be performed because the resulting collation is unresolved due to collation conflict.

Implicit conversion of %ls value to %ls cannot be performed because the collation of the value is unresolved due to a collation conflict.

Table 3–6:TSQL Error Codes - 500 to 599

Description

The SQL Debugging Interface (SDI) requires that SQL Server, when started as a service, must not log on as System Account. Reset to log on as user account using Control Panel.

Unable to send symbol information to debugger on %ls for connection %d. Debugging disabled.

Unable to connect to debugger on %ls (Error = 0x%08x). Ensure that client-side components, such as SQLDBREG.EXE, are installed and registered on %.*ls. Debugging disabled for connection %d.

Current user account was invoked with SETUSER. Changing databases is not allowed.

Invalid escape character '%.*ls' was specified in a LIKE predicate.

Invalid argument for SET ROWCOUNT. Must be a non-null non-negative integer.

Unable to connect to debugger on %ls (Error = 0x%08x). Ensure that client-side components, such as SQLLE.DLL, are installed and registered on %.*ls. Debugging disabled for connection %d.

User name '%.*ls' not found.

Cannot create a worktable row larger than allowable maximum. Resubmit your query with the ROBUST PLAN hint.

Cannot create a row of size %d which is greater than the allowable maximum of %d.

Subquery returned more than 1 value. This is not permitted when the subquery follows =, !=, <, <= , >, >= or when the subquery is used as an expression.

A column insert or update conflicts with a rule imposed by a previous CREATE RULE statement.The statement was terminated.The conflict occurred in database '%.*ls', table '%.*ls', column '%.*ls'.

Unable to communicate with debugger on %ls (Error = 0x%08x). Debugging disabled for connection %d.

Description

Cannot insert the value NULL into column '%.*ls', table '%.*ls'; column does not
allow nulls. %ls fails.

Attempt to initialize OLE library failed. Check for correct versions of OLE DLLs on this machine.

Adding a value to a '%ls' column caused overflow.

Cannot convert data type %ls to %ls.

SQL Server no longer supports version %d of the SQL Debugging Interface (SDI).

System error detected during attempt to use the 'upsleep' system function.

Explicit conversion from data type %ls to %ls is not allowed.

The timestamp (changed to %S_TS) shows that the row has been updated by another user.

Difference of two datetime columns caused overflow at runtime.

Invalid length parameter passed to the substring function.

Cannot find '%.*ls'. This language may have been dropped. Contact your system administrator.

An invalid datetime value was encountered. Value exceeds the year 9999.

Cannot insert explicit value for identity column in table '%.*ls' when IDENTITY_INSERT is set to OFF.

Explicit value must be specified for identity column in table '%.*ls' when IDENTITY_INSERT is set to ON.

%ls statement conflicted with %ls %ls constraint '%.*ls'. The conflict occurred in database '%.*ls', table '%.*ls'%ls%.*ls%ls.

The identity range managed by replication is full and must be updated by a replication agent. The %ls conflict occurred in database '%.*ls', table '%.*ls'%ls%.*ls%ls. Sp_adjustpublisheridentityrange can be called to get a new identity range.

The attempted insert or update failed because the target view either specifies
WITH CHECK OPTION or spans a view that specifies WITH CHECK OPTION
and one or more rows resulting from the operation did not qualify under the CHECK OPTION constraint.

The checksum has changed to %d. This shows that the row has been updated by another user.

CryptoAPI function '%ls' failed. Error 0x%x: %ls

User-defined functions are not yet enabled.

INSERT EXEC failed because the stored procedure altered the schema of the
target table.

Only functions and extended stored procedures can be executed from within a function.

Remote function calls are not allowed within a function.

Description

Failed to access file '%.*ls'

Failed to access file '%.*ls'. Files can be accessed only through shares

The transaction for the INSERT EXEC statement has been rolled back. The
INSERT EXEC operation will be terminated.

Attempted to create a record with a fixed length of '%d'. Maximum allowable fixed length is '%d'.

The server encountered a stack overflow during compile time.

Error writing audit trace. SQL Server is shutting down.

File '%.*ls' either does not exist or is not a recognizable trace file. Or there was an error opening the file.

Server encountered an error '%.*ls'.

Table 3–7:TSQL Error Codes - 600 to 699

Description

Could not continue scan with NOLOCK due to data movement.

Could not find row in sysindexes for database ID %d, object ID %ld, index ID %d. Run DBCC CHECKTABLE on sysindexes.

Could not find row in sysobjects for object ID %ld in database '%.*ls'. Run DBCC CHECKTABLE on sysobjects.

Attempt to fetch logical page %S_PGID in database '%.*ls' belongs to object '%.*ls', not to object '%.*ls'.

Insufficient room was allocated for search arguments in the session descriptor for object '%.*ls'. Only %d search arguments were anticipated.

Could not find database table ID %d, name '%.*ls'.

Descriptor for object ID %ld in database ID %d not found in the hash table during attempt to unhash it.

A varno of %d was passed to the opentable system function. The largest valid value is %d.

Filegroup '%.*ls' has no files assigned to it. Tables, indexes, and text, ntext, and image columns cannot be populated on this filegroup until a file is added.

Could not retrieve row from page by RID because logical page %S_PGID is not a data page. %S_RID. %S_PAGE.

Could not retrieve row from page by RID because the requested RID has a higher number than the last RID on the page. %S_RID.%S_PAGE, DBID %d.

Cannot retrieve row from page %S_PGID by RID because the slotid (%d) is not valid.

Cannot use ROLLBACK with a savepoint within a distributed transaction.

Cannot use SAVE TRANSACTION within a distributed transaction.

Cannot issue SAVE TRANSACTION when there is no active transaction.

Description

Process %d tried to remove DES resource lock %S_DES, which it does not hold.

Index shrink program returned invalid status of 0.

Could not fetch logical page %S_PGID, database ID %d. The page is not currently allocated.

Could not find the index entry for RID '%.*hs' in index page %S_PGID, index ID %d, database '%.*ls'.

Could not find the clustered index entry for page %S_PGID, object ID %ld, status 0x%x. Index page %S_PGID, in database '%.*ls', was searched for this entry.

You can only specify the READPAST lock in the READ COMMITTED or REPEATABLE READ isolation levels.

Cannot use %hs granularity hint on table '%.*ls' because locking at the specified granularity is inhibited.

Index ID %d for table '%.*ls' resides on a read-only filegroup which cannot be modified.

Two buffers are conflicting for the same keep slot in table '%.*ls'.

No slots are free to keep buffers for table '%.*ls'.

Expected to find buffer in keep slot for table '%.*ls'.

Maximum system-generated unique value for a duplicate group exceeded for
table ID %d, index ID %d. Dropping and re-creating the index may fix the problem;
otherwise use another clustering key.

Index %d for table '%.*ls' resides on offline filegroup that cannot be accessed.

Table 3–8:TSQL Error Codes - 700 to 799

Description

There is insufficient system memory to run this query.

Warning: Due to low virtual memory, special reserved memory used %d times since startup. Increase virtual memory on server.

Table 3–9:TSQL Error Codes - 800 to 899

Description

No more buffers can be stolen.

Could not find buffer 0x%lx holding logical page %S_PGID in the SDES 0x%lx kept buffer pool for object '%.*ls'.

Buffer 0x%lx, allocation page %S_PGID, in database '%.*ls' is not in allocation buffer pool in PSS (process status structure). Contact Technical Support.

Logical page %S_PGID in database ID %d is already hashed.

Process ID %d tried to remove a buffer resource lock %S_BUF that it does not hold in SDES %S_SDES. Contact Technical Support.

Description

There is no room to hold the buffer resource lock %S_BUF in SDES %S_SDES. Contact Technical Support.

Could not unhash buffer at 0x%lx with a buffer page number of %S_PGID and database ID %d with HASHED status set. The buffer was not found. %S_PAGE.

Could not start I/O for request %S_BLKIOPTR.

I/O error %ls detected during %S_MSG at offset %#016I64x in file '%ls'.

The bufclean system function was called on dirty buffer (page %S_PGID, stat %#x/%#x, objid %#x, sstat%#x).

Device '%.*ls' (physical name '%.*ls', virtual device number %d) is not available. Contact the system administrator for assistance.

Time out occurred while waiting for buffer latch type %d, bp %#x, page %S_PGID, stat %#x, object ID %d:%d:%d, waittime %d. Continuing to wait.

Time-out occurred while waiting for buffer latch type %d for page %S_PGID, database ID %d.

Table 3–10:TSQL Error Codes - 900 to 999

Description

Could not find descriptor for database ID %d, object ID %ld in hash table after hashing it.

To change the %ls, the database must be in state in which a checkpoint can be executed.

Could not find row in sysindexes for clustered index on system catalog %ld in database ID %d. This index should exist in all databases. Run DBCC CHECKTABLE on sysindexes in the database.

Could not locate row in sysobjects for system catalog '%.*ls' in database '%.*ls'. This system catalog should exist in all databases. Run DBCC CHECKTABLE on sysobjects in this database.

Could not locate entry in sysdatabases for database '%.*ls'. No entry found with that name. Make sure that the name is entered correctly.

Could not find database ID %d. Database may not be activated yet or may be in transition.

Server user '%.*ls' is not a valid user in database '%.*ls'.

Database '%.*ls' has not been recovered yet. Wait and try again.

Database '%.*ls' is being recovered. Waiting until recovery is finished.

Database '%.*ls' is in restricted mode. Only the database owner and members of the dbcreator and sysadmin roles can access it.

Database '%.*ls' is already open and can only have one user at a time.

Maximum number of databases used for each query has been exceeded. The maximum allowed is %d.

Description

Database '%.*ls' cannot be opened. It has been marked SUSPECT by recovery. See the SQL Server errorlog for more information.

Database '%.*ls' cannot be opened. It is in the middle of a restore.

Attempting to close a database that is not already open. Contact Technical
Support.

Cannot open database '%.*ls'. It has not been upgraded to the latest format.

Database '%.*ls' cannot be opened because it is offline.

Database '%.*ls' cannot be opened because its version (%d) is later than the current server version (%d).

Converting database '%.*ls' from version %d to the current version %d.

Database '%.*ls' cannot be opened due to inaccessible files or insufficient memory or disk space. See the SQL Server errorlog for details.

Cannot open database '%.*ls' version %d. Upgrade the database to the latest version.

Error while closing database '%.*ls' cleanly.

Database '%.*ls' cannot be upgraded. Database is version %d and this server supports version %d.

tempdb is skipped. You cannot run a query that requires tempdb

Database '%.*ls' cannot be upgraded - database has a version (%d) earlier than SQL Server 7.0(%d).

Database '%.*ls' running the upgrade step from version %d to version %d.

Database '%.*ls' is in transition. Try the statement later.

Warning: Index '%ls' on '%ls' in database '%ls' may be corrupt because of expression evaluation changes in this release. Drop and re-create the index.

Table 3–11:TSQL Error Codes - 1000 to 1099

Description

Line %d: Length or precision specification %d is invalid.

Line %d: Specified scale %d is invalid.

Line %d: %ls clause allowed only for %ls.

Invalid column prefix '%.*ls': No table name specified

Line %d: Invalid procedure number (%d). Must be between 1 and 32767.

CREATE TRIGGER contains no statements.

The %S_MSG '%.*ls' is out of the range for numeric representation (maximum precision 38).

The SELECT item identified by the ORDER BY number %d contains a variable as part of the expression identifying a column position. Variables are only allowed when ordering by an expression referencing a column name.

Description

Invalid escape character '%.*ls'.

The correlation name '%.*ls' is specified multiple times in a FROM clause.

The correlation name '%.*ls' has the same exposed name as table '%.*ls'.

Tables or functions '%.*ls' and '%.*ls' have the same exposed names. Use correlation names to distinguish them.

TOP clause contains an invalid value.

An aggregate cannot appear in an ON clause unless it is in a subquery contained in a HAVING clause or select list, and the column being aggregated is an outer reference.

Outer join operators cannot be specified in a query containing joined tables.

Invalid column list after object name in GRANT/REVOKE statement.

Column list cannot be specified for object-level permissions.

FIPS Warning: Line %d has the non-ANSI statement '%ls'.

FIPS Warning: Line %d has the non-ANSI clause '%ls'.

Invalid parameter %d specified for %ls.

FIPS Warning: Line %d has the non-ANSI function '%ls'.

FIPS Warning: The length of identifier '%.*ls' exceeds 18.

Too many expressions are specified in the GROUP BY clause. The maximum number is %d when either CUBE or ROLLUP is specified.

The CUBE and ROLLUP options are not allowed in a GROUP BY ALL clause.

Browse mode is invalid for subqueries and derived tables.

Percent values must be between 0 and 100.

Cannot use the column prefix '%.*ls'. This must match the object in the UPDATE clause '%.*ls'.

The ORDER BY clause is invalid in views, inline functions, derived tables, and subqueries, unless TOP is also specified.

Incorrect syntax near '%.*ls', expected '%.*ls'.

File option %hs is required in this CREATE/ALTER DATABASE statement.

The CASCADE, WITH GRANT or AS options cannot be specified with statement permissions.

Cannot use empty object or column names. Use a single space if necessary.

Option '%.*ls' is specified more than once.

Mixing old and new syntax in CREATE/ALTER DATABASE statement is not allowed.

Option %.*ls is not allowed for a LOG file.

Conflicting %ls optimizer hints specified.

Description

'%hs' is not yet implemented.

Cannot use an existing function name to specify a stored procedure name.

Aggregates are not allowed in this context. Only scalar expressions are allowed.

Subqueries are not allowed in this context. Only scalar expressions are allowed.

Conflicting locking hints specified.

Conflicting cursor options %ls and %ls.

Mixing old and new syntax to specify cursor options is not allowed.

This syntax is only allowed within the stored procedure sp_executesql.

Cursor parameters in a stored procedure must be declared with OUTPUT and VARYING options, and they must be specified in the order CURSOR VARYING
OUTPUT.

Conflicting %ls options %ls and %ls.

For DROP STATISTICS, you must give both the table and the column name in the form 'tablename.column'.

Syntax '%ls' is not allowed in schema-bound objects.

'%.*ls' is an invalid name because it contains a NULL character.

The maximum number of elements in the select list is %d and you have supplied %d.

The IDENTITY function cannot be used with a SELECT INTO statement containing a UNION operator.

Cannot specify both READ_ONLY and FOR READ ONLY on a cursor declaration.

Cannot set or reset the %ls option within a procedure.

The number of rows in the TOP clause must be an integer.

The text/ntext/image constants are not yet implemented.

The TOP N WITH TIES clause is not allowed without a corresponding ORDER BY clause.

A filegroup cannot be added using ALTER DATABASE ADD FILE. Use ALTER DATABASE ADD FILEGROUP.

A filegroup cannot be used with log files.

The NOLOCK, READUNCOMMITTED, and READPAST lock hints are only allowed in a SELECT statement.

Warning. Line %d: The option '%ls' is obsolete and has no effect.

The SET SHOWPLAN statements must be the only statements in the batch.

Only one list of index hints per table is allowed.

Index hints are only allowed in a FROM clause.

CREATE INDEX option '%.*ls' is no longer supported.

Description

Cannot specify a JOIN algorithm with a remote JOIN.

A REMOTE hint can only be specified with an INNER JOIN clause.

'%.*ls' is not a recognized cursor option for cursor %.*ls.

Creation of temporary functions is not allowed.

RETURN statements in scalar valued functions must include an argument.

Function '%s' requires at least %d argument(s).

INSERT into an identity column not allowed on table variables.

'%.*ls %.*ls' is not a recognized option.

A variable cannot be used to specify a search condition in a fulltext predicate when accessed through a cursor.

Table 3–12:TSQL Error Codes - 1100 to 1199

Description

Could not allocate new page for database '%.*ls'. There are no more pages available in filegroup %.*ls. Space can be created by dropping objects, adding additional files, or allowing file growth.

IAM page %S_PGID for object ID %ld is incorrect. The %S_MSG ID on page is
%ld; should be %ld. The entry in sysindexes may be incorrect or the IAM page
may contain an error.

Allocation page %S_PGID in database '%.*ls' has different segment ID than that of the object which is being allocated to. Run DBCC CHECKALLOC.

Could not allocate space for object '%.*ls' in database '%.*ls' because the '%.*ls' filegroup is full.

Could not read allocation page %S_PGID because either the object ID (%ld) is not correct, or the page ID (%S_PGID) is not correct.

Table 3–13:TSQL Error Codes - 1200 to 1299

Description

The page_lock system function was called with a mode %d that is not permitted.

Process ID %d attempting to unlock unowned resource %.*ls.

The SQL Server cannot obtain a LOCK resource at this time. Rerun your statement when there are fewer active users or ask the system administrator to check the SQL Server lock and memory configuration.

Transaction (Process ID %d) was deadlocked on {%Z} resources with another
process and has been chosen as the deadlock victim. Rerun the transaction.

Transaction manager has canceled the distributed transaction.

Process ID %d was chosen as the deadlock victim with P_BACKOUT bit set.

No more lock classes available from transaction.

Invalid lock class for release call.

Lock request time out period exceeded.

Attempting to release application lock '%.*ls' that is not currently held.

Table 3–14:TSQL Error Codes - 1500 to 1599

Description

Sort failure.

CREATE UNIQUE INDEX terminated because a duplicate key was found for
index ID %d. Most significant primary key is '%S_KEY'.

Warning: Deleted duplicate row. Primary key is '%S_KEY'.

CREATE INDEX terminated because a duplicate row was found. Primary key is
'%S_KEY'.

Row compare failure.

Sort failed. Out of space or locks in database '%.*ls'.

Sort cannot be reconciled with transaction log.

Sort failure. Prevented overwriting of allocation page in database '%.*ls' by terminating sort.

Sort failure. Prevented incorrect extent deallocation by aborting sort.

Character data comparison failure. An unrecognized Sort-Map-Element type (%d) was found in the server-wide default sort table at SMEL entry [%d].

Character data comparison failure. A list of Sort-Map-Elements from the server-wide default sort table does not end properly. This list begins at SMEL entry [%d].

CREATE INDEX with DROP_EXISTING was aborted because a row was out of
order. Most significant offending primary key is '%S_KEY'. Explicitly drop and create the index instead.

Description

The SORTED_DATA_REORG option cannot be used for a nonclustered index if the keys are not unique within the table. CREATE INDEX was aborted because of duplicate keys. Primary key is '%S_KEY'.

New sort run starting on page %S_PGID found extent not marked as shared.

Cannot share extent %S_PGID among more than eight sort runs.

Extent %S_PGID not found in shared extent directory.

Cannot share extent %S_PGID with shared extent directory full.

Cannot build a nonclustered index on a memory-only work table.

Cannot suspend a sort not in row input phase.

Cannot insert into a sort not in row input phase.

Cannot sort a row of size %d, which is greater than the allowable maximum of %d.

Table 3–15:TSQL Error Codes - 1600 to 1699

Description

Could not open tempdb. Cannot continue.

Cannot start C2 audit trace. SQL Server is shutting down.

Server started with '-f'. Auditing will not be started.

Table 3–16:TSQL Error Codes - 1700 to 1799

Description

Creation of table '%.*ls' failed because the row size would be %d, including internal overhead. This exceeds the maximum allowable table row size, %d.

CREATE TABLE failed because column '%.*ls' in table '%.*ls' exceeds the
maximum of %d columns.

Could not allocate disk space for a work table in database '%.*ls'. You may be able to free up space by using BACKUP LOG, or you may want to extend the size of the database by using ALTER DATABASE.

Only members of the sysadmin role can create the system table '%.*ls'.

You must create system table '%.*ls' in the master database.

System table '%.*ls' was not created, because ad hoc updates to system catalogs are not enabled.

Warning: The table '%.*ls' has been created but its maximum row size (%d) exceeds the maximum number of bytes per row (%d). INSERT or UPDATE of a row in this table will fail if the resulting row length exceeds %d bytes.

Cannot use TEXTIMAGE_ON when a table has no text, ntext, or image columns.

Could not create constraint. See previous errors.

Description

Could not create DEFAULT for column '%.*ls' as it is not a valid column in the table '%.*ls'.

Column '%.*ls.%.*ls' is not the same length as referencing column '%.*ls.%.*ls' in foreign key '%.*ls'.

Defaults cannot be created on columns with an IDENTITY attribute. Table '%.*ls', column '%.*ls'.

Defaults cannot be created on columns of data type timestamp. Table '%.*ls', column '%.*ls'.

Skipping FOREIGN KEY constraint '%.*ls' definition for temporary table.

Column '%.*ls.%.*ls' is not of same collation as referencing column '%.*ls.%.*ls' in foreign key '%.*ls'.

Invalid column '%.*ls' is specified in a constraint or computed-column definition.

Constraints of type %ls cannot be created on columns of type %ls.

Cross-database foreign key references are not supported. Foreign key '%.*ls'.

Foreign key references to temporary tables are not supported. Foreign key '%.*ls'.

Foreign key '%.*ls' references invalid table '%.*ls'.

Foreign key '%.*ls' references object '%.*ls' which is not a user table.

Foreign key '%.*ls' references invalid column '%.*ls' in referencing table '%.*ls'.

Foreign key '%.*ls' references invalid column '%.*ls' in referenced table '%.*ls'.

Foreign key '%.*ls' defines an invalid relationship between a user table and system table.

Foreign key '%.*ls' has implicit reference to object '%.*ls' which does not have a primary key defined on it.

The number of columns in the referencing column list for foreign key '%.*ls' does not match those of the primary key in the referenced table '%.*ls'.

There are no primary or candidate keys in the referenced table '%.*ls' that match the referencing column list in the foreign key '%.*ls'.

User does not have correct permissions on referenced table '%.*ls' to create foreign key '%.*ls'.

Column '%.*ls.%.*ls' is not the same data type as referencing column '%.*ls.%.*ls' in foreign key '%.*ls'.

Table '%.*ls' already has a primary key defined on it.

Could not find column ID %d in syscolumns for object ID %d in database ID %d.

Column already has a DEFAULT bound to it.

Cannot create the foreign key '%.*ls' because the referenced column '%.*ls.%.*ls' is a computed column.

Description

Introducing FOREIGN KEY constraint '%.*ls' on table '%.*ls' may cause cycles or multiple cascade paths. Specify ON DELETE NO ACTION or ON UPDATE NO ACTION, or modify other FOREIGN KEY constraints.

Either column '%.*ls.%.*ls' or referencing column '%.*ls.%.*ls' in foreign key '%.*ls' is a timestamp column. This data type cannot be used with cascading referential integrity constraints.

Cannot define foreign key constraint '%.*ls' with cascaded DELETE or UPDATE on table '%.*ls' because the table has an INSTEAD OF DELETE or UPDATE TRIGGER defined on it.

Cascading foreign key '%.*ls' cannot be created where the referencing column '%.*ls.%.*ls' is an identity column.

Cannot use CHECKSUM(*) in a computed column definition.

Table 3–17:TSQL Error Codes - 1800 to 1899

Description

Database '%.*ls' already exists.

CREATE DATABASE failed. Some file names listed could not be created. Check
previous errors.

CREATE DATABASE failed. Could not allocate enough disk space for a new
database on the named disks. Total space allocated must be at least %d MB to accommodate a copy of the model database.

There is no disk named '%.*ls'. Checking other disk names.

The CREATE DATABASE process is allocating %.2f MB on disk '%.*ls'.

CREATE DATABASE failed. The default collation of database '%.*ls' cannot be
set to '%.*ls'.

Could not obtain exclusive lock on database '%.*ls'. Retry the operation later.

Default devices are not supported.

To achieve optimal performance, update all statistics on the '%.*ls' database by running sp_updatestats.

'%.*ls' is the wrong type of device for CREATE DATABASE or ALTER DATABASE. Check sysdevices. The statement is aborted.

CREATE DATABASE failed. COLLATE clause cannot be used with the FOR
ATTACH option.

Could not open new database '%.*ls'. CREATE DATABASE is aborted.

Could not create tempdb. If space is low, extend the amount of space and restart.

Primary log file '%ls' is missing and the database was not cleanly shut down so it cannot be rebuilt.

Could not create default log file because the name was too long.

Disk '%.*ls' is already completely used by other databases. It can be expanded with DISK RESIZE.

Description

User-defined filegroups are not allowed on '%hs'.

CREATE/ALTER DATABASE failed because the resulting cumulative database
size would exceed your licensed limit of %d MB per %S_MSG.

The file named '%.*ls' is already in use. Choose another name.

The FOR ATTACH option requires that at least the primary file be specified.

The files '%.*ls' and '%.*ls' are both primary files. A database can only have one primary file.

Could not attach database '%.*ls' to file '%.*ls'.

File '%ls' cannot be reused until after the next BACKUP LOG operation.

The file '%ls' cannot be overwritten. It is being used by database '%.*ls'.

Unable to create/attach any new database because the number of existing databases has reached the maximum number allowed: %d.

Table 3–18:TSQL Error Codes - 1900 to 1999

Description

Column '%.*ls'. Cannot create index on a column of bit data type.

Cannot create more than one clustered index on table '%.*ls'. Drop the existing clustered index '%.*ls' before creating another.

Index keys are too large. The %d bytes needed to represent the keys for index %d exceeds the size limit of %d bytes.

Cannot specify more than %d column names for statistics or index key list. %d specified.

Could not find 'zero' row for index '%.*ls' the table in sysindexes.

Cannot create an index on '%.*ls', because this table does not exist in database '%.*ls'.

Cannot re-create index '%.*ls'. The new index definition does not match the constraint being enforced by the existing index.

Cannot use duplicate column names in index key list. Column name '%.*ls' listed more than once.

Cannot create more than %d nonclustered indices or column statistics on one table.

Column name '%.*ls' does not exist in the target table.

There is already an index on table '%.*ls' named '%.*ls'.

Index cannot be created on object '%.*ls' because the object is not a user table or view.

CREATE INDEX options %ls and %ls are mutually exclusive.

Index (ID = %d) is being rebuilt.

Description

Column '%.*ls'. Cannot create index on a column of text, ntext, or image data type.

Skipping rebuild of index ID %d, which is on a read-only filegroup.

Invalid filegroup '%.*ls' specified.

Filegroup '%.*ls' has no files assigned to it. Tables, indexes, and text, ntext, and image columns cannot be created on this filegroup.

The clustered index has been dropped.

Filegroup '%.*ls' is read-only.

Cannot convert a clustered index to a nonclustered index using the
DROP_EXISTING option.

Cannot create a clustered index because nonclustered index ID %d is on a read-only filegroup.

There are already statistics on table '%.*ls' named '%.*ls'.

Cannot create statistics on table '%.*ls' because this table does not exist in database '%.*ls'.

Statistics cannot be created on object '%.*ls' because the object is not a user table or view.

Filegroup '%.*ls' is offline.

Cannot create a clustered index because nonclustered index ID %d is on an offline filegroup.

Cannot create index because the key column '%.*ls' is non-deterministic or imprecise.

%ls failed because the following SET options have incorrect settings: '%.*ls'.

Cannot create index. Object '%.*ls' was created with the following SET options off: '%.*ls'.

Cannot %ls the %S_MSG '%.*ls'. It contains one or more disallowed constructs.

Cannot index the view '%.*ls'. It references another view or function '%.*ls'.

Index cannot be created on %S_MSG '%.*ls' because the underlying object '%.*ls' has a different owner.

Cannot create %S_MSG on view '%.*ls' because the view is not schema bound.

Cannot create %S_MSG on view '%.*ls'. It does not have a unique clustered index.

Nonunique clustered index cannot be created on view '%.*ls' because only unique clustered indexes are allowed.

Index cannot be created on view '%.*ls' because the view contains text, ntext or image columns.

Index cannot be created on view '%.*ls' because the view has one or more nondeterministic expressions.

Description

Index '%.*ls' was not created. This index has a key length of at least %d bytes. The maximum permissible key length is %d bytes.

Warning! The maximum key length is %d bytes. The index '%.*ls' has maximum length of %d bytes. For some combination of large values, the insert/update operation will fail.

Operation failed. The index entry of length %d bytes for the index '%.*ls' exceeds the maximum length of %d bytes.

Index cannot be created on view '%.*ls' because the view contains a self-join on '%.*ls'.

Duplicate index names '%.*ls' and '%.*ls' detected on table '%.*ls'.

Index on view '%.*ls' cannot be created because function '%s' yields nondeterministic results.

Index on view '%.*ls' cannot be created because the view contains an imprecise expression in a GROUP BY clause

Index on view '%.*ls' cannot be created because the view contains an imprecise expression in the WHERE clause.

Index on view '%.*ls' cannot be created because the view contains an imprecise expression in a join.

Index on view '%.*ls' cannot be created because some arguments are missing in a built-in function.

Index on view '%.*ls' cannot be created because the view uses a column bound to a rule.

Index on view '%.*ls' cannot be created because the view contains a nondeterministic computed column.

Index on view '%.*ls' cannot be created because the view uses a nondeterministic user-defined function.

Index on view '%.*ls' cannot be created because the view requires a conversion involving dates or variants.

This edition of SQL Server does not support indexed views.

Cannot create index on view or computed column because this database is not SQL Server compatible.

Table 3–19:TSQL Error Codes - 2000 to 2099

Description

Cannot use duplicate parameter names. Parameter name '%.*ls' listed more than once.

Procedure '%.*ls' has already been created with group number %d. Create procedure with an unused group number.

Cannot add rows to sysdepends for the current stored procedure because it depends on the missing object '%.*ls'. The stored procedure will still be created.

The object '%.*ls' is not a procedure so you cannot create another procedure under that group name.

Procedure '%.*ls' was created despite delayed name resolution warnings (if any).

Cannot perform alter on %.*ls because it is an incompatible object type.

Index hints cannot be specified within a schema-bound object.

User-defined variables cannot be declared within a schema-bound object.

Table 3–20:TSQL Error Codes - 2100 to 2199

Description

Cannot create a trigger on table '%.*ls', because this table does not exist in database '%.*ls'.

Cannot create a trigger on table '%.*ls' because you can only create a trigger on a table in the current database.

Cannot alter trigger '%.*ls' for table '%.*ls' because this trigger does not belong to this table.

Cannot %s trigger '%.*ls' for %S_MSG '%.*ls' because an INSTEAD OF %s trigger already exists.

Cannot %s trigger '%.*ls' for view '%.*ls' because it is defined with the CHECK
OPTION.

Cannot %s INSTEAD OF DELETE or UPDATE TRIGGER '%.*ls' on table '%.*ls' because the table has a FOREIGN KEY with cascaded DELETE or UPDATE.

Column '%.*ls' cannot be used in an IF UPDATE clause because it is a computed column.

Table 3–21:TSQL Error Codes - 2500 to 2599

Description

Could not find a table or object named '%.*ls'. Check sysobjects.

Could not start transaction.

Successfully deleted the physical file '%ls'.

Could not delete the physical file '%ls'. The DeleteFile system function returned error %ls.

Description

The device '%.*ls' does not exist. Use sp_helpdevice to show available devices.

Could not find a table or object name '%.*ls' in database '%.*ls'.

Table error: Object ID %d, Index ID %d. Keys out of order on page %S_PGID, slots %d and %d.

Table error: Object ID %d, Index ID %d. Duplicate keys on page %S_PGID slot %d and page %S_PGID slot %d.

Table error: Object ID %ld (object '%.*ls') does not match between '%.*ls' and '%.*ls'.

Table error: Data type %ld (type '%.*ls') does not match between '%.*ls' and '%.*ls'.

Page %S_PGID, object ID %d, index ID %d has been modified but is not marked modified in the differential backup bitmap.

The differential bitmap was invalidated for database %.*ls. A full database backup is required before a differential backup can be performed.

The minimally logged operation status has been turned on for database %.*ls. Rerun backup log operations to ensure that all data has been secured.

Unable to process table %.*ls because filegroup %.*ls is invalid.

Could not find database '%.*ls'. Check sysdatabases.

Could not find database ID %d. Check sysdatabases.

Unable to process index %.*ls of table %.*ls because filegroup %.*ls is invalid.

Filegroup %.*ls is invalid.

Unable to process table %.*ls because filegroup %.*ls is offline.

Database file %.*ls is offline.

Incorrect DBCC statement. Check the documentation for the correct DBCC syntax and options.

Unable to process index %.*ls of table %.*ls because filegroup %.*ls is offline.

DBCC execution completed. If DBCC printed error messages, contact your system administrator.

Filegroup %.*ls is offline.

Secondary index entries were missing or did not match the data in the table. Use the WITH TABLOCK option and run the command again to display the failing records.

Table error: Object ID %d, index ID %d B-tree level mismatch, page %S_PGID. Level %d does not match level %d from previous %S_PGID.

DBCC SHRINKFILE could not shrink file %ls. Log files are not supported.

Table error: Page %S_PGID allocated to object ID %d, index ID %d was not seen. Page may be invalid or have incorrect object ID information in its header.

Description

Table error: Page %S_PGID with object ID %d, index ID %d in its header is allocated by another object.

Table error: Page %S_PGID is allocated to object ID %d, index ID %d, not to object ID %d, index ID %d found in page header.

DBCC results for '%.*ls'.

Table error: Object ID %d, index ID %d, page %S_PGID, row %d. Record check (%hs) failed. Values are %ld and %ld.

File %d. Number of extents = %ld, used pages = %ld, reserved pages = %ld.

Total number of extents = %ld, used pages = %ld, reserved pages = %ld in this database.

The system cannot self repair this error.

DBCC UPDATEUSAGE: sysindexes row updated for table '%.*ls' (index ID %ld):

DATA pages: Changed from (%ld) to (%ld) pages.

USED pages: Changed from (%ld) to (%ld) pages.

RSVD pages: Changed from (%ld) to (%ld) pages.

ROWS count: Changed from (%I64d) to (%I64d) rows.

Index '%.*ls' on table '%.*ls' is marked offline. Rebuild the index to bring it online.

Performing second pass of index checks.

DBCC: Compaction phase of index '%.*ls' is %d%% complete.

DBCC: Defrag phase of index '%.*ls' is %d%% complete.

User '%.*ls' does not have permission to run DBCC %ls for object '%.*ls'.

The '%ls' and '%ls' options are not allowed on the same statement.

Parameter %d is incorrect for this DBCC statement.

'%ls' cannot access object '%.*ls' because it is not a table.

DBCC DBREINDEX cannot be used on system tables.

DBCC INDEXDEFRAG cannot be used on system table indexes

Page %S_PGID is out of range for this database or is in a log file.

Warning: Page %S_PGID, slot %d in Object %d Index %d Column %.*ls value %.*ls is out of range for data type "%.*ls". Update column to a legal value.

User '%.*ls' does not have permission to run DBCC %.*ls.

DBCC cannot free DLL '%.*ls'. The DLL is in use.

Database '%.*ls' is not marked suspect. You cannot drop it with DBCC.

Object ID %d, index ID %d: Page %S_PGID is empty. This is not permitted at level %d of the B-tree.

IAM page %S_PGID is pointed to by the next pointer of IAM page %S_PGID object ID %d index ID %d but was not detected in the scan.

Description

IAM page %S_PGID is pointed to by the previous pointer of IAM page %S_PGID object ID %d index ID %d but was not detected in the scan.

Chain sequence numbers are out of order in IAM chain for object ID %d, index ID %d. Page %S_PGID sequence number %d points to page %S_PGID sequence number %d.

Minimally logged extents were found in GAM interval starting at page %S_PGID but the minimally logged flag is not set in the database table.

Table error: Extent %S_PGID object ID %d, index ID %d is beyond the range of this database.

Table '%.*ls' is either a system or temporary table. DBCC CLEANTABLE cannot be applied to a system or temporary table.

An incorrect number of parameters was given to the DBCC statement.

Page %S_PGID was expected to be the first page of a text, ntext, or image value.

User '%.*ls' is modifying bytes %d to %d of page %S_PGID in database '%.*ls'.

Could not find row in sysindexes with index ID %d for table '%.*ls'.

%ls index successfully restored for object '%.*ls' in database '%.*ls'.

There are %I64d rows in %ld pages for object '%.*ls'.

Invalid index ID (%d) specified.

Database '%.*ls' must be set to single user mode before executing this statement.

The database is not open. Execute a 'USE %.*ls' statement and rerun the DBCC statement.

Clustered indexes on sysobjects and sysindexes cannot be re-created.

Table 3–22:TSQL Error Codes - 2600 to 2699

Description

Cannot insert duplicate key row in object '%.*ls' with unique index '%.*ls'.

No space left on logical page %S_PGID of index ID %d for object '%.*ls' when inserting row on an index page. This situation should have been handled while traversing the index.

Buffer holding logical page %S_PGID not found in keep pool in SDES for object '%.*ls'. Contact Technical Support.

Could not insert into table %S_DES because row length %d is less than the minimum length %d.

Violation of %ls constraint '%.*ls'. Cannot insert duplicate key in object '%.*ls'.

Table 3–23:TSQL Error Codes - 2700 to 2799

Description

Database name '%.*ls' ignored, referencing object in tempdb.

Description

Database '%.*ls' does not exist.

Column names in each table must be unique. Column name '%.*ls' in table '%.*ls' is specified more than once.

Table '%.*ls' does not exist.

You are not the owner specified for the object '%.*ls' in this statement (CREATE,
ALTER, TRUNCATE, UPDATE STATISTICS or BULK INSERT).

There is already an object named '%.*ls' in the database.

Column or parameter #%d: Cannot find data type %.*ls.

Column or parameter #%d: Cannot specify a column width on data type %.*ls.

The size (%d) given to the %S_MSG '%.*ls' exceeds the maximum allowed (%d).

Column or parameter #%d: Cannot specify null values on a column of data type bit.

Could not find a default segment to create the table on. Ask your system administrator to specify a default segment in syssegments.

Parameter '%.*ls' has an invalid data type.

Cannot find index '%.*ls'.

Cannot create procedure '%.*ls' with a group number of %d because a procedure with the same name and a group number of 1 does not currently exist in the
database. Must execute CREATE PROCEDURE '%.*ls';1 first.

Column '%.*ls' has invalid width: %d.

Error number %ld is invalid. The number must be from %ld through %ld

The user name '%.*ls' does not exist in sysusers.

Owner name specified is a group name. Objects cannot be owned by groups.

Message passed to %hs must be of type char, varchar, nchar, or nvarchar.

A table can only have one timestamp column. Because table '%.*ls' already has one, the column '%.*ls' cannot be added.

The text, ntext, and image data types are invalid for local variables.

SET LANGUAGE failed because '%.*ls' is not an official language name or a language alias on this SQL Server.

SET DATEFORMAT date order '%.*ls' is invalid.

SET DATEFIRST %d is out of range.

%ls statement requires %S_MSG parameter.

Multiple identity columns specified for table '%.*ls'. Only one identity column per table is allowed.

Process ID %d has raised user error %d, severity %d. SQL Server is terminating this process.

Cannot specify user error format string with a length exceeding %d bytes.

Description

Too many substitution parameters for RAISERROR. Cannot exceed %d substitution parameters.

Cannot specify %ls data type (RAISERROR parameter %d) as a substitution parameter for RAISERRROR.

Identity column '%.*ls' must be of data type int, bigint, smallint, tinyint, or decimal or numeric with a scale of 0, and constrained to be nonnullable.

Column or parameter #%d: Specified column precision %d is greater than the maximum precision of %d.

Column or parameter #%d: Specified column scale %d is greater than the specified precision of %d.

Identity column '%.*ls' contains invalid SEED.

Identity column '%.*ls' contains invalid INCREMENT.

Error severity levels greater than %d can only be specified by members of the sysadmin role, using the WITH LOG option.

SET DEADLOCK_PRIORITY option '%.*ls' is invalid.

Invalid value %d for state. Valid range is from %d to %d.

RAISERROR failed due to invalid parameter substitution(s) for error %d, severity %d, state %d.

%hs could not locate entry for error %d in sysmessages.

CREATE SCHEMA failed due to previous errors.

Specified owner name '%.*ls' either does not exist or you do not have permission to use it.

The ROWGUIDCOL property can only be specified on the uniqueidentifier data type.

sp_setapprole was not invoked correctly. Refer to the documentation for more information.

Could not find application role '%.*ls'.

Incorrect password supplied for application role '%.*ls'.

Could not locate statistics for column '%.*ls' in the system catalogs.

The definition for user-defined data type '%.*ls' has changed.

Could not locate statistics '%.*ls' in the system catalogs.

Statistics for %ls '%.*ls'.

Column '%.*ls'. Cannot create statistics on a column of data type %ls.

The SELECT INTO statement cannot have same source and destination tables.

Cannot create statistics on table '%.*ls'. This table is a virtual system table.

Cannot access temporary tables from within a function.

Sort order ID %d is invalid.

Description

Collation ID %d is invalid.

Code page %d is not supported by the operating system.

Database '%.*ls' contains columns or parameters with the following code page(s) not supported by the operating system: %ls.

Table 3–24:TSQL Error Codes - 2800 to 2899

Description

The definition of object '%.*ls' has changed since it was compiled.

The request for %S_MSG '%.*ls' failed because '%.*ls' is a %S_MSG object.

Could not find stored procedure '%.*ls'.

Table 3–25:TSQL Error Codes - 3000 to 3099

Description

Could not insert a backup or restore history/detail record in the msdb database. This may indicate a problem with the msdb database.The backup/restore operation was still successful.

All backup devices must be of the same general class (for example, DISK and
TAPE).

%hs is terminating abnormally.

%hs successfully processed %d pages in %d.%03d seconds (%d.%03d MB/sec).

%hs is not yet implemented.

File '%ls' of database '%ls' has been removed or shrunk since this backup or restore operation was interrupted. The operation cannot be restarted.

Could not resume interrupted backup or restore operation. See the SQL Server error log for more information.

There is no interrupted backup or restore operation to restart. Reissue the statement without the RESTART clause.

The checkpoint file was for a different backup or restore operation. Reissue the statement without the RESTART clause.

The backup operation cannot be restarted as the log has been truncated. Reissue the statement without the RESTART clause.

Cannot perform a backup or restore operation within a transaction.

Backup and file manipulation operations (such as ALTER DATABASE ADD FILE) on a database must be serialized. Reissue the statement after the current backup or file manipulation operation is completed.

You can only perform a full backup of the master database. Use BACKUP DATABASE to back up the entire master database.

Missing database name. Reissue the statement specifying a valid database name.

Description

Could not find filegroup ID %d in sysfilegroups for database '%ls'.

Could not find filegroup '%.*ls' in sysfilegroups for database '%.*ls'.

Operation checkpoint file is invalid. Could not restart operation. Reissue the statement without the RESTART option.

Option '%ls' conflicts with option(s) '%ls'. Remove the conflicting option and reissue the statement.

One or more of the options (%ls) are not supported for this statement. Review the documentation for supported options.

BACKUP DATABASE cannot be used on a database opened in emergency mode.

No files were selected to be processed. You may have selected one or more filegroups that have no members.

Cannot perform a differential backup for database '%ls', because a current database backup does not exist. Perform a full database backup by reissuing BACKUP DATABASE, omitting the WITH DIFFERENTIAL option.

Database '%ls' is in warm-standby state (set by executing RESTORE WITH STANDBY) and cannot be backed up until the entire load sequence is completed.

Minimally logged operations have occurred prior to this WITH RESTART command. Reissue the BACKUP statement without WITH RESTART.

The filename '%ls' is invalid as a backup device name. Reissue the BACKUP statement with a valid filename.

Cannot perform a differential backup for file '%ls' because a current file backup does not exist. Reissue BACKUP DATABASE omitting the WITH DIFFERENTIAL option.

An error occurred while informing replication of the backup. The backup will continue, but the replication environment should be inspected.

BACKUP failed to complete the command %.*ls

Table 3–26:TSQL Error Codes - 3100 to 3199

Description

Exclusive access could not be obtained because the database is in use.

RESTORE DATABASE must be used in single user mode when trying to restore the master database.

User does not have permission to RESTORE database '%.*ls'.

Cannot restore any database other than master when the server is in single user mode.

The database owner (DBO) does not have an entry in sysusers in database '%.*ls'.

Database '%.*ls' does not have an entry in sysdatabases.

Invalid database name '%.*ls' specified for backup or restore operation.

Temporary Message: The backup set does not contain pages for file '%ls'.

Description

File '%ls' has an unsupported page size (%d).

Temporary Message: File '%ls' has changed size from %d to %d bytes.

The media set for database '%ls' has %d family members but only %d are provided. All members must be provided.

The volume on device '%ls' is not a member of the media family.

The backup set in file '%ls' was created by %hs and cannot be used for this restore operation.

Cannot apply the backup on device '%ls' to database '%ls'.

One or more files in the backup set are no longer part of database '%ls'.

Could not adjust the space allocation for file '%ls'.

The database to be restored was named '%ls'. Reissue the statement using the
WITH REPLACE option to overwrite the '%ls' database.

File '%ls' cannot be restored over the existing '%ls'. Reissue the RESTORE statement using WITH REPLACE to overwrite pre-existing files.

The data set on device '%ls' is not a SQL Server backup set.

File '%.*ls' was not backed up in file %d on device '%ls'. The file cannot be restored from this backup set.

The STOPAT option is not supported for RESTORE DATABASE. You can use the STOPAT option with RESTORE LOG.

None of the newly-restored files had been modified after the backup was taken, so no further recovery actions are required. The database is now available for use.

Backup and restore operations are not allowed on database tempdb.

Media recovery for ALTER DATABASE is not yet implemented. The database cannot be rolled forward.

The master database has been successfully restored. Shutting down SQL Server.

The master database failed to restore. Use the rebuildm utility to rebuild the master database. Shutting down SQL Server.

Cannot overwrite file '%ls' because it is marked as read-only.

The database is already fully recovered.

The backup set holds a backup of a database other than the existing '%ls' database.

The RESTORE operation cannot proceed because one or more files have been added or dropped from the database since the backup set was created.

File '%ls' cannot be restored to '%ls'. Use WITH MOVE to identify a valid location for the file.

The logical file (%d) is named '%ls'. RESTORE will not overwrite it from '%ls'.

Description

Could not create one or more files. Consider using the WITH MOVE option to identify valid locations.

The tail of the log for database '%ls' has not been backed up. Back up the log and rerun the RESTORE statement specifying the FILE clause.

Could not update primary file information in sysdatabases.

The primary file is unavailable. It must be restored or otherwise made available.

The database has on-disk structure version %d. The server supports version %d and can only restore such a database that was inactive when it was backed up. This database was not inactive.

The transaction log was damaged. All data files must be restored before RESTORE LOG can be attempted.

Cannot roll forward the database with on-disk structure version %d. The server supports version %d. Reissue the RESTORE statement WITH RECOVERY.

Could not adjust the replication state of database '%ls'. The database was successfully restored, however its replication state is indeterminate. See the Troubleshooting Replication section in SQL Server Books Online.

RESTORE DATABASE could not drop database '%ls'. Drop the database and then reissue the RESTORE DATABASE statement.

RESTORE could not start database '%ls'.

The backup of the system database on device %ls cannot be restored because it was created by a different version of the server (%u) than this server (%u).

The backed-up database has on-disk structure version %d. The server supports version %d and cannot restore or upgrade this database.

The STANDBY filename is invalid.

Cannot restore file %ls because the file is offline.

Cannot restore filegroup %ls because the filegroup is offline.

The file '%ls' cannot be moved by this RESTORE operation.

The filegroup '%ls' cannot be restored because all of the files are not present in the backup set. File '%ls' is missing.

File '%ls' is claimed by '%ls'(%d) and '%ls'(%d). The WITH MOVE clause can be used to relocate one or more files.

Only members of the dbcreator and sysadmin roles can execute the %ls statement.

File %ls is not in the correct state to have this differential backup applied to it.

The system database cannot be moved by RESTORE.

This backup cannot be restored using WITH STANDBY because a database upgrade is needed. Reissue the RESTORE without WITH STANDBY.

Table 3–27:TSQL Error Codes - 3200 to 3299

Description

Cannot open backup device '%ls'. Device error or device off-line. See the SQL Server error log for more details.

Write on '%ls' failed, status = %ld. See the SQL Server error log for more details.

Read on '%ls' failed, status = %ld. See the SQL Server error log for more details.

Operator aborted backup or restore. See the error messages returned to the console for more details.

Too many backup devices specified for backup or restore; only %d are allowed.

No entry in sysdevices for backup device '%.*ls'. Update sysdevices and rerun statement.

Backup or restore requires at least one backup device. Rerun your statement specifying a backup device.

Unexpected end of file while reading beginning of backup set. Confirm that the media contains a valid SQL Server backup set, and see the console error log for more details.

'%.*ls' is not a backup device. Check sysdevices.

%d percent %hs.

Invalid value specified for %ls parameter.

The ReadFileEx system function executed on file '%ls' only read %d bytes, expected %d.

The WriteFileEx system function executed on file '%ls' only wrote %d bytes, expected %d.

Cannot create worker thread.

The volume on device '%ls' is a duplicate of stripe set member %d.

Request for device '%ls' timed out.

Operation on device '%ls' exceeded retry count.

Logical file '%.*ls' is not part of database '%ls'. Use RESTORE FILELISTONLY to list the logical file names.

File '%ls' is not part of database '%ls'. You can only list files that are members of this database.

Option not supported for Named Pipe-based backup sets.

The backup set on device '%ls' uses a feature of the Microsoft Tape Format not supported by SQL Server.

The media family on device '%ls' is incorrectly formed. SQL Server cannot process this media family.

The file on device '%ls' is not a valid Microsoft Tape Format backup set.

The media family on device '%ls' was created using Microsoft Tape Format version %d.%d. SQL Server supports version %d.%d.

Description

Descriptor block size exceeds %d bytes. Use a shorter name and/or description string and retry the operation.

Could not convert a string to or from Unicode, %ls.

The media family on device '%ls' is marked as nonappendable. Reissue the statement using the INIT option to overwrite the media.

The volume on device '%ls' has the wrong media sequence number (%d). Remove it and insert volume %d.

>>> VOLUME SWITCH <<< (not for output!)

The volume on device '%ls' is a continuation volume for the backup set. Remove it and insert the volume holding the start of the backup set.

The value '%d' is not within range for the %ls parameter.

The media family on device '%ls' is complete. The device is now being reused for one of the remaining families.

The block size parameter must supply a value that is a power of 2.

The volume on device '%ls' is empty.

The data set on device '%ls' is a SQL Server backup set not compatible with this version of SQL Server.

The backup set on device '%ls' was terminated while it was being created and is incomplete. RESTORE sequence is terminated abnormally.

There is insufficient free space on disk volume '%ls' to create the database. The database requires %I64u additional free bytes, while only %I64u bytes are available.

The volume on device '%ls' belongs to a different media set.

The volume on device '%ls' is not part of a multiple family media set. BACKUP
WITH FORMAT can be used to form a new media set.

An internal buffer has become full.

SQL Server cannot use the virtual device configuration.

The backup set is valid.

Cannot use the volume on device '%ls' as a continuation volume. It is sequence number %d of family %d for the current media set. Insert a new volume, or sequence number %d of family %d for the current set.

The operation did not proceed far enough to allow RESTART. Reissue the statement without the RESTART qualifier.

The login has insufficient authority. Membership of the sysadmin role is required to use VIRTUAL_DEVICE with BACKUP or RESTORE.

The backup data in '%ls' is incorrectly formatted. Backups cannot be appended, but existing backup sets may still be usable.

Insufficient resources to create UMS scheduler.

Description

Cannot use the backup file '%ls' because it was originally formatted with sector size %d and is now on a device with sector size %d.

Cannot restore the file '%ls' because it was originally written with sector size %d;
'%ls' is now on a device with sector size %d.

An internal consistency error occurred. Contact Technical Support for assistance.

Nonrecoverable I/O error occurred on file '%ls'.

The '%ls' device has a hardware sector size of %d, but the block size parameter specifies an incompatible override value of %d. Reissue the statement using a compatible block size.

The BUFFERCOUNT parameter must supply a value that allows at least one buffer per backup device.

Incorrect checksum computed for the backup set on device %ls. The backup set cannot be restored.

I/O request 0x%08x failed I/O verification. See the error log for a description.

WITH SNAPSHOT can be used only if the backup set was created WITH
SNAPSHOT.

WITH SNAPSHOT must be used with only one virtual device.

Failed to encrypt string %ls

Access is denied due to a password failure

Backups on raw devices are not supported. '%ls' is a raw device.

Released and initiated rewind on '%ls'.

Table 3–28:TSQL Error Codes - 3300 to 3399

Description

Invalid log record found in the transaction log (logop %d).

Error while redoing logged operation in database '%.*ls'. Error at log record ID
%S_LSN.

Error while undoing logged operation in database '%.*ls'. Error at log record ID
%S_LSN.

During rollback, process %d was expected to hold mode %d lock at level %d for row %S_RID in database '%.*ls' under transaction %S_XID.

Table 3–29:TSQL Error Codes - 3400 to 3499

Description

Recovering database '%.*ls'.

%d transactions rolled forward in database '%.*ls' (%d).

%d transactions rolled back in database '%.*ls' (%d).

Recovery complete.

Description

Database ID %d. Could not mark database as suspect. Getnext NC scan on sysdatabases.dbid failed.

Database '%.*ls' (database ID %d) could not recover. Contact Technical Support.

Database '%.*ls' is read-only or has read-only files and must be made writable before it can be upgraded.

Cannot recover the master database. Exiting.

Warning: The outcome of transaction %S_XID, named '%.*ls' in database '%.*ls' (database ID %d), could not be determined because the coordinating database (database ID %d) could not be opened. The transaction was assumed to be committed.

Warning: Could not determine the outcome of transaction %S_XID, named '%.*ls' in database '%.*ls' (with ID %d) because the coordinating database (ID %d) did not contain the outcome. The transaction was assumed to be committed.

Could not recover database '%.*ls' (database ID %d) due to unresolved transaction outcomes.

Warning: syslanguages is missing.

Name is truncated to '%.*ls'. The maximum name length is %d.

Cannot change sort order or locale. Server shutting down. Restart SQL Server to continue with sort order unchanged.

Sort order or locale cannot be changed because user objects or user databases exist.

Cannot rebuild index for the '%.*ls' table in the '%.*ls' database.

Error recovering database '%.*ls'. Could not connect to MSDTC to check the completion status of transaction %S_XID.

Database '%.*ls' (database ID %d) failed to recover because transaction first LSN is not equal to LSN in checkpoint. Contact Technical Support.

Database '%.*ls' (database ID %d). The DBCC RECOVERDB statement failed due to previous errors.

Database '%.*ls' (database ID %d). The DBCC RECOVERDB statement can only be run after a RESTORE statement that used the WITH NORECOVERY option.

Database '%.*ls' (database ID %d). The RESTORE statement could not access file '%ls'. Error was '%ls'.

Database '%.*ls' (database ID %d). The size of the undo file is insufficient.

Database '%.*ls' (database ID %d) was marked for standby or read-only use, but has been modified. The RESTORE LOG statement cannot be performed.

File '%ls' is not a valid undo file for database '%.*ls', database ID %d.

Primary log file is not available for database '%.*ls'. The log cannot be backed up.

Could not activate or scan all of the log files for database '%.*ls'.

Description

Could not undo log record %S_LSN, for transaction ID %S_XID, on page %S_PGID, database '%.*ls' (database ID %d). Page information: LSN = %S_LSN, type = %ld. Log information: OpCode = %ld, context %ld.

An error has occurred that requires SQL Server to shut down so that recovery can be performed on database ID %d.

Recovery of database '%.*ls' (%d) is %d%% complete (approximately %d more seconds) (Phase %d of 3).

Recovery has failed because reexecution of CREATE INDEX found inconsistencies between target filegroup '%ls' (%d) and source filegroup '%ls' (%d). Restore both filegroups before attempting further RESTORE LOG operations.

Recovery of database '%.*ls' (%d) detected possible identity value inconsistency in table ID %d. Run DBCC CHECKIDENT ('%.*ls').

This version cannot redo any index creation or non-logged operation done by SQL Server 7.0. Further roll forward is not possible.

Recovery is checkpointing database '%.*ls' (%d)

Analysis of database '%.*ls' (%d) is %d%% complete (approximately %d more seconds)

Could not redo log record %S_LSN, for transaction ID %S_XID, on page %S_PGID, database '%.*ls' (%d). Page: LSN = %S_LSN, type = %ld. Log: OpCode = %ld, context %ld, PrevPageLSN: %S_LSN.

Table 3–30:TSQL Error Codes - 3500 to 3599

Description

Could not find row in sysdatabases for database ID %d at checkpoint time.

Only the owner of database '%.*ls' can run the CHECKPOINT statement.

Could not get an exclusive lock on the database '%.*ls'. Make sure that no other users are currently using this database, and rerun the CHECKPOINT statement.

Could not set database '%.*ls' %ls read-only user mode because you could not exclusively lock the database.

Database '%.*ls' cannot be changed from read-only because the primary and/or log file(s) are not writable.

Table 3–31:TSQL Error Codes - 3600 to 3699

Description

Duplicate key was ignored.

Duplicate row was ignored.

Arithmetic overflow occurred.

Division by zero occurred.

Cannot allocate a GUID for the token.

Description

%hsSQL Server Execution Times:%hs CPU time = %lu ms, elapsed time = %lu ms.

SQL Server parse and compile time: %hs CPU time = %lu ms, elapsed time = %lu ms.

Table '%.*ls'. Scan count %d, logical reads %d, physical reads %d, read-ahead reads %d.

The transaction has been terminated.

Could not write a CHECKPOINT record in database ID %d because the log is out of space.

Automatic checkpointing is disabled in database '%.*ls' because the log is out of space. It will continue when the database owner successfully checkpoints the database. Free up some space or extend the database and then run the CHECKPOINT statement.

The statement has been terminated.

A domain error occurred.

'%hs' is not yet implemented.

Could not create worker thread.

A floating point exception occurred in the user process. Current transaction is canceled.

This SQL Server has been optimized for %d concurrent queries. This limit has been exceeded by %d queries and performance may be adversely affected.

Concurrency violations since %ls%s 1 2 3 4 5 6 7 8 9 10-100 >100%s%6u%6u%6u%6u%6u%6u%6u%6u%6u%8u%6u

Concurrency violations will be written to the SQL Server error log.

Concurrency violations will not be written to the SQL Server error log.

Table 3–32:TSQL Error Codes - 3700 to 3799

Description

Cannot %S_MSG the %S_MSG '%.*ls', because it does not exist in the system catalog.

Cannot drop the %S_MSG '%.*ls' because it is currently in use.

Cannot detach the %S_MSG '%.*ls' because it is currently in use.

User does not have permission to perform this operation on %S_MSG '%.*ls'.

Cannot use DROP %ls with '%.*ls' because '%.*ls' is a %S_MSG. Use DROP %ls.

Cannot %S_MSG the %S_MSG '%.*ls' because it is a system %S_MSG.

The %S_MSG '%.*ls' cannot be dropped because it is bound to one or more
%S_MSG.

Description

Could not drop index '%.*ls' because the table or clustered index entry cannot be found in the sysindexes system table.

An explicit DROP INDEX is not allowed on index '%.*ls'. It is being used for %ls constraint enforcement.

Cannot %S_MSG the %S_MSG '%.*ls' because it is being used for replication.

The constraint '%.*ls' is being referenced by table '%.*ls', foreign key constraint '%.*ls'.

Could not drop object '%.*ls' because it is referenced by a FOREIGN KEY constraint.

Could not drop constraint. See previous errors.

'%.*ls' is not a constraint.

Cannot %ls '%.*ls' because it is being referenced by object '%.*ls'.

Constraint '%.*ls' does not belong to table '%.*ls'.

Cannot drop the %S_MSG '%.*ls' because it is being used for distribution.

Could not delete file '%ls'. See the SQL Server error log for more information.

Deleting database file '%ls'.

Cannot %ls the index '%.*ls' because it is not a statistics collection.

Cannot drop the %S_MSG '%.*ls' because at least part of the table resides on a read-only filegroup.

Cannot drop the %S_MSG '%.*ls' because at least part of the table resides on an offline filegroup.

Table 3–33:TSQL Error Codes - 3900 to 3999

Description

The COMMIT TRANSACTION request has no corresponding BEGIN
TRANSACTION.

The ROLLBACK TRANSACTION request has no corresponding BEGIN
TRANSACTION.

Cannot unsplit logical page %S_PGID in object '%.*ls', in database '%.*ls'. Both pages together contain more data than will fit on one page.

Could not run BEGIN TRANSACTION in database '%.*ls' because the database is read-only.

Could not run BEGIN TRANSACTION in database '%.*ls' because the database is in bypass recovery mode.

Session binding token is invalid.

Transaction context in use by another session.

Cannot bind using an XP token while the server is not in an XP call.

Description

The data type '%s' is invalid for transaction names or savepoint names. Allowed data types are char, varchar, nchar, or nvarchar.

Cannot use the ROLLBACK statement within an INSERT-EXEC statement.

Cannot use the COMMIT statement within an INSERT-EXEC statement unless BEGIN TRANSACTION is used first.

Session is bound to a transaction context that is in use. Other statements in the batch were ignored.

Statement must be executed in the context of a user transaction.

Cannot enlist in the transaction because the transaction has already been committed or rolled back.

The WITH MARK option only applies to the first BEGIN TRAN WITH MARK statement. The option is ignored.

Cannot get a transaction token if there is no transaction active. Reissue the statement after a transaction has been started

Cannot enlist in the transaction because the transaction does not exist.

Cannot use transaction marks on database '%.*ls' with bulk-logged operations that have not been backed up. The mark is ignored.

The session was enlisted in an active user transaction while trying to bind to a new transaction. The session has defected from the previous user transaction.

Invalid transaction mark name. The 'LSN:' prefix is reserved.

The transaction active in this session has been committed or aborted by another session.

The session had an active transaction when it tried to enlist in a Distributed Transaction Coordinator transaction.

The marked transaction '%.*ls' failed. A Deadlock was encountered while attempting to place the mark in the log.

Table 3–34:TSQL Error Codes - 4000 to 4099

Description

ODS error. Server is terminating this connection.

Unicode data in a Unicode-only collation or ntext data cannot be sent to clients using DB-Library (such as ISQL) or ODBC version 3.7 or earlier.

Language requested in login '%.*ls' is not an official name on this SQL Server. Using server-wide default %.*ls instead.

Language requested in 'login %.*ls' is not an official name on this SQL Server. Using user default %.*ls instead.

Neither the language requested in 'login %.*ls' nor user default language %.*ls is an official language name on this SQL Server. Using server-wide default %.*ls instead.

Description

User default language %.*ls is not an official language name on this SQL Server. Using server-wide default %.*ls instead.

Language requested in login '%.*ls' is not an official language name on this SQL Server. Login fails.

Default date order '%.*ls' for language %.*ls is invalid. Using mdy instead.

Mount tape for %hs of database '%ls'.

End of tape has been reached. Remove tape '%ls' and mount next tape for %hs of database '%ls'.

The medium on device '%ls' expires on %hs and cannot be overwritten.

Processed %d pages for database '%ls', file '%ls' on file %d.

User-specified volume ID '%ls' does not match the volume ID '%ls' of the device '%ls'.

Cannot find file ID %d on device '%ls'.

Cannot open database requested in login '%.*ls'. Login fails.

Cannot open either database requested in login (%.*ls) or user default database. Using master database instead.

Cannot open user default database. Using master database instead.

Cannot open database requested in login (%.*ls). Using user default '%.*ls' instead.

Cannot open user default database. Login failed.

Table 3–35:TSQL Error Codes - 4200 to 4299

Description

The statement %hs is not allowed while the recovery model is SIMPLE. Use BACKUP DATABASE or change the recovery model using ALTER DATABASE.

Cannot back up the log of the master database. Use BACKUP DATABASE instead.

There is no current database backup. This log backup cannot be used to roll forward a preceding database backup.

The log was not truncated because records at the beginning of the log are pending replication. Ensure the Log Reader Agent is running or use sp_repldone to mark transactions as distributed.

Minimally logged operations cannot be backed up when the database is unavailable.

BACKUP LOG cannot modify the database because database is read-only. The backup will continue,although subsequent backups will duplicate the work of this backup.

Table 3–36:TSQL Error Codes - 4300 to 4399

Description

Database in use. The system administrator must have exclusive use of the database to restore the log.

A USER ATTENTION signal raised during RESTORE LOG is being ignored until the current restore completes.

The log in this backup set begins at LSN %.*ls, which is too late to apply to the database. An earlier log backup that includes LSN %.*ls can be restored.

The preceding restore operation did not specify WITH NORECOVERY or WITH STANDBY. Restart the restore sequence, specifying WITH NORECOVERY or
WITH STANDBY for all but the final step.

Can only RESTORE LOG in the master database if SQL Server is in single user mode.

File '%ls' has been rolled forward to LSN %.*ls. This log terminates at LSN %.*ls, which is too early to apply the WITH RECOVERY option. Reissue the RESTORE LOG statement WITH NORECOVERY.

File '%ls' was only partially restored by a database or file restore. The entire file must be successfully restored before applying the log.

This log file contains records logged before the designated point-in-time. The database is being left in load state so you can apply another log file.

The database is marked suspect. Transaction logs cannot be restored. Use RESTORE DATABASE to recover the database.

Backup history older than %ls has been deleted.

Could not delete entries for backup set ID '%ls'.

The log in this backup set terminates at LSN %.*ls, which is too early to apply to the database. A more recent log backup that includes LSN %.*ls can be restored.

The log in this backup set contains minimally logged changes. Point-in-time recovery is inhibited. RESTORE will roll forward to end of logs without recovering the database.

File '%ls' is missing. Rollforward stops at log sequence number %.*ls. File is created at LSN %.*ls, dropped at LSN %.*ls. Restore transaction log beyond beyond point in time when file was dropped or restore data to be consistent with rest of database.

This log file contains records logged before the designated mark. The database is being left in load state so you can apply another log file.

The log in this backup set cannot be applied because it is on a recovery path inconsistent with the database.

The database cannot be recovered because the files have been restored to inconsistent points in time.

RESTORE LOG has been halted. To use the database in its current state, run RESTORE DATABASE %ls WITH RECOVERY.

The database cannot be recovered because the log was not restored.

Description

The named mark does not identify a valid LSN.

Table 3–37:TSQL Error Codes - 4400 to 4499

Description

View or function '%.*ls' is not updatable because it contains aggregates.

View or function '%.*ls' is not updatable because the definition contains the DISTINCT clause.

View or function '%.*ls' is not updatable because the modification affects multiple base tables.

Update or insert of view or function '%.*ls' failed because it contains a derived or constant field.

The query and the views or functions in it exceed the limit of %d tables.

Could not use view or function '%.*ls' because of binding errors.

Could not allocate ancillary table for view or function resolution. The maximum number of tables in a query (%d) was exceeded.

View '%.*ls' is not updatable because either it was created WITH CHECK OPTION or it spans a view created WITH CHECK OPTION and the target table is referenced multiple times in the resulting query.

UNION ALL view '%.*ls' is not updatable because the definition contains a disallowed construct.

Derived table '%.*ls' is not updatable because the definition contains a UNION operator.

Derived table '%.*ls' is not updatable because it contains aggregates.

Derived table '%.*ls' is not updatable because the definition contains the DISTINCT clause.

Derived table '%.*ls' is not updatable because the modification affects multiple base tables.

Derived table '%.*ls' is not updatable because a column of the derived table is derived or constant.

View '%.*ls' has an INSTEAD OF UPDATE trigger and cannot be a target of an
UPDATE FROM statement.

View '%.*ls' has an INSTEAD OF DELETE trigger and cannot be a target of a
DELETE FROM statement.

Joined tables cannot be specified in a query containing outer join operators. View or function '%.*ls' contains joined tables.

Cannot specify outer join operators in a query containing joined tables. View or function '%.*ls' contains outer join operators.

The view or function '%.*ls' is not updatable because the definition contains the TOP clause.

Description

The derived table '%.*ls' is not updatable because the definition contains the TOP clause.

View or function '%.*ls' contains a self-reference. Views or functions cannot reference themselves directly or indirectly.

Warning: Index hints supplied for view '%.*ls' will be ignored.

Partitioned view '%.*ls' is not updatable because table '%.*ls' has a timestamp column.

Partitioned view '%.*ls' is not updatable because table '%.*ls' has a DEFAULT constraint.

Cannot INSERT into partitioned view '%.*ls' because table '%.*ls' has an IDENTITY constraint.

Partitioned view '%.*ls' is not updatable because table '%.*ls' has an INSTEAD OF trigger.

Partitioned view '%.*ls' is not updatable because a value was not specified for partitioning column '%.*ls'.

UNION ALL view '%.*ls' is not updatable because a partitioning column was not found.

Partitioned view '%.*ls' is not updatable as the target of a bulk operation.

Partitioned view '%.*ls' is not updatable because it does not deliver all columns from its member tables.

Partitioned view '%.*ls' is not updatable because the source query contains references to partition table '%.*ls'.

UNION ALL view '%.*ls' is not updatable because a primary key was not found on table '%.*ls'.

Partitioned view '%.*ls' is not updatable because the table '%.*ls' has an index on a computed column.

UNION ALL view '%.*ls' is not updatable because base table '%.*ls' is used multiple times.

UNION ALL view '%.*ls' is not updatable because column '%.*ls' of base table '%.*ls' is used multiple times.

UNION ALL view '%.*ls' is not updatable because the primary key of table '%.*ls' is not included in the union result.

UNION ALL view '%.*ls' is not updatable because the primary key of table '%.*ls' is not unioned with primary keys of preceding tables.

UNION ALL view '%.*ls' is not updatable because the definiton of column '%.*ls' of view '%.*ls' is used by another view column.

View '%.*ls' is not updatable because the definition contains a set operator.

Cannot INSERT into partitioned view '%.*ls' because values were not supplied for all columns.

Using defaults is not allowed in views that contain a set operator.

Description

Cannot update partitioned view '%.*ls' because the definition of the view column '%.*ls' in table '%.*ls' has a IDENTITY constraint.

Views referencing tables on multiple servers are not updatable on this SKU of
SQL Server.

Cannot UPDATE partitioning column '%.*ls' of view '%.*ls' because the table '%.*ls' has a CASCADE DELETE or CASCADE UPDATE constraint.

Cannot UPDATE partitioning column '%.*ls' of view '%.*ls' because the table '%.*ls' has a INSERT, UPDATE or DELETE trigger.

Table 3–38:TSQL Error Codes - 4500 to 4599

Description

View or function '%.*ls' has more columns defined than column names given.

View or function '%.*ls' has more column names specified than columns defined.

CREATE VIEW failed because column '%.*ls' in view '%.*ls' exceeds the maximum
of %d columns.

Column names in each view or function must be unique. Column name '%.*ls' in view or function '%.*ls' is specified more than once.

Views or functions are not allowed on temporary tables. Table names that begin with '#' denote temporary tables.

Could not perform CREATE VIEW because WITH %ls was specified and the view contains set operators.

Could not perform CREATE VIEW because WITH %ls was specified and the view is not updatable.

Create View or Function failed because no column name was specified for column %d.

Cannot schema bind %S_MSG '%.*ls' because name '%.*ls' is invalid for schema binding. Names must be in two-part format and an object cannot reference itself.

Cannot schema bind %S_MSG '%.*ls'. '%.*ls' is not schema bound.

CREATE FUNCTION failed because a column name is not specified for column
%d.

CREATE FUNCTION failed because column '%.*ls' in function '%.*ls' exceeds
the maximum of %d columns.

Cannot schema bind function '%.*ls' because it contains an EXECUTE statement.

Table 3–39:TSQL Error Codes - 4600 to 4699

Description

Only members of the sysadmin role can grant or revoke the CREATE DATABASE permission.

There is no such user or group '%.*ls'.

Granted or revoked privilege %ls is not compatible with object.

You can only grant or revoke permissions on objects in the current database.

To revoke grantable privileges, specify the CASCADE option with REVOKE.

Grantor does not have GRANT permission.

Invalid column name '%.*ls'.

Cannot grant, deny or revoke permissions to or from special roles.

You do not have permission to use %.*ls in the AS clause.

CREATE DATABASE permission can only be granted in the master database.

Table 3–40:TSQL Error Codes - 4700 to 4799

Description

Could not truncate table '%.*ls' because this table does not exist in database '%.*ls'.

Could not truncate table '%.*ls' because there is not enough room in the log to record the deallocation of all the index and data pages.

Could not truncate object '%.*ls' because it or one of its indexes resides on a READONLY filegroup.

Could not truncate object '%.*ls' because it is not a table.

You are not allowed to truncate the system table '%.*ls'.

Cannot truncate table '%.*ls' because it is published for replication.

Cannot truncate table '%.*ls' because it is being referenced by a FOREIGN KEY constraint.

Table 3–41:TSQL Error Codes - 4800 to 4899

Description

Received invalid row length %d from bcp client. Maximum row size is %d.

Premature end-of-message while reading current row from host. Host program may have terminated.

The front-end tool you are using does not support the feature of bulk insert from host. Use the proper tools for this command.

Received invalid row length %d from bcp client. Minimum row size is %d.

Bulk copy operations cannot trigger BULK INSERT statements.

Description

Expected the TEXT token in data stream for bulk copy of text or image data.

Expected the column offset in data stream for bulk copy of text or image data.

Expected the row offset in data stream for bulk copy of text or image data.

Expected the text length in data stream for bulk copy of text, ntext, or image data.

Received invalid column length from bcp client.

Could not bulk insert. Invalid sorted column '%.*ls'. Assuming data stream is not sorted.

Could not bulk insert. Sorted column '%.*ls' was specified more than once. Assuming data stream is not sorted.

Could not bulk insert. Bulk data stream was incorrectly specified as sorted.

Could not bulk insert. Unknown version of format file '%s'.

Could not bulk insert. Error reading the number of columns from format file '%s'.

Could not bulk insert. Invalid number of columns in format file '%s'.

Could not bulk insert. Invalid column number in format file '%s'.

Could not bulk insert. Invalid data type for column number %d in format file '%s'.

Could not bulk insert. Invalid prefix for column number %d in format file '%s'.

Could not bulk insert. Invalid column length for column number %d in format file '%s'.

Could not bulk insert. Invalid column terminator for column number %d in format file '%s'.

Could not bulk insert. Invalid destination table column number for source column %d in format file '%s'.

Could not bulk insert. Error reading destination table column name for source column %d in format file '%s'.

Bulk Insert: DataFileType was incorrectly specified as char. DataFileType will be assumed to be widechar because the data file has a Unicode signature.

Bulk Insert: DataFileType was incorrectly specified as widechar. DataFileType will be assumed to be char because the data file does not have a Unicode signature.

Bulk Insert: Unexpected end-of-file (EOF) encountered in data file.

Bulk Insert: Version mismatch between the provider dynamic link library and the server executable.

You do not have permission to use the BULK INSERT statement.

Bulk copying into a table with computed columns is not supported for downlevel clients.

Error: Cannot bulk copy into a table '%s' enabled for immediate-updating subscriptions

Description

The bulk data source does not support the SQLNUMERIC or SQLDECIMAL data types.

Cannot perform bulk insert. Invalid collation name for source column %d in format file '%s'.

The bulk data source provider string has an invalid %ls property value %ls.

The data source name is not a simple object name.

The required FormatFile property is missing from the provider string of the server.

The bulk data source provider string has a syntax error ('%lc') near character position %d.

The bulk data source provider string has an unsupported property name (%ls).

The bulk data source provider string has a syntax error near character position %d. Expected '%lc', but found '%lc'.

The bulk data provider failed to allocate memory.

Bulk copying into a table with bigint columns is not supported for versions earlier than SQL Server 2000.

Bulk copying into a table with sql_variant columns is not supported for versions earlier than SQL Server 2000.

Could not import table '%ls'. Error %d.

Data import: Table '%ls' is already locked by another user.

Data import: Table '%ls' already has data. Skipping to next table.

Data import: Table '%ls' does not exist or it is not a user table.

%hs

%hs

Could not bulk insert. File '%ls' does not exist.

Could not bulk insert because file '%ls' could not be opened. Operating system error code %ls.

Could not bulk insert because file '%ls' could not be read. Operating system error code %ls.

Bulk insert data conversion error (truncation) for row %d, column %d (%ls).

Bulk insert data conversion error (type mismatch) for row %d, column %d (%ls).

Could not bulk insert because the maximum number of errors (%d) was exceeded.

Bulk Insert fails. Column is too long in the data file for row %d, column %d. Make sure the field terminator and row terminator are specified correctly.

Bulk insert data conversion error (overflow) for row %d, column %d (%ls).

Bulk Insert fails. Codepage '%d' is not installed. Install the codepage and run the command again.

Description

Bulk Insert failed. Unexpected NULL value in data file row %d, column %d. Destination column (%ls) is defined NOT NULL.

Could not bulk insert. When using the FIRSTROW and LASTROW parameters, the value for FIRSTROW cannot be greater than the value for LASTROW.

Note: Bulk Insert through a view may result in base table default values being ignored for NULL columns in the data file.

Could not bulk insert. Prefix length, field length, or terminator required for source column %d in format file '%s'.

Table 3–42:TSQL Error Codes - 4900 to 4999

Description

ALTER TABLE only allows columns to be added that can contain nulls or have
a DEFAULT definition specified. Column '%.*ls' cannot be added to table '%.*ls' because it does not allow nulls and does not specify a DEFAULT definition.

Cannot alter table '%.*ls' because this table does not exist in database '%.*ls'.

Cannot alter '%.*ls' because it is not a table.

Only the owner or members of the sysadmin role can alter table '%.*ls'.

Could not enable or disable the constraint. See previous errors.

Constraint '%.*ls' does not exist.

ALTER TABLE failed because trigger '%.*ls' on table '%.*ls' does not exist.

ALTER TABLE failed because trigger '%.*ls' does not belong to table '%.*ls'.

%ls %.*ls failed because one or more objects access this column.

ALTER TABLE DROP COLUMN failed because '%.*ls' is the only data column
in table '%.*ls'. A table must have at least one data column.

%ls failed because column '%.*ls' does not exist in table '%.*ls'.

ALTER TABLE ALTER COLUMN ADD ROWGUIDCOL failed because a column
already exists in table '%.*ls' with ROWGUIDCOL property.

ALTER TABLE ALTER COLUMN DROP ROWGUIDCOL failed because a column
does not exist in table '%.*ls' with ROWGUIDCOL property.

Cannot alter column '%.*ls' to be data type %.*ls.

Cannot alter column '%.*ls' because it is '%ls'.

Cannot alter the %S_MSG '%.*ls' because it is being published for replication.

Warning: Columns added to the replicated table %S_MSG '%.*ls' will be ignored by existing articles.

Cannot add columns to %S_MSG '%.*ls' because it is being published for merge replication.

ALTER TABLE DROP COLUMN failed because '%.*ls' is currently replicated.

Table 3–43:TSQL Error Codes - 5000 to 5099

Description

User must be in the master database.

Database '%.*ls' does not exist. Check sysdatabases.

To use ALTER DATABASE, the database must be in a writable state in which a checkpoint can be executed.

Extending database by %.2f MB on disk '%.*ls'.

Could not get exclusive use of %S_MSG '%.*ls' to perform the requested operation.

This ALTER DATABASE statement is not supported.

ALTER DATABASE failed. Some disk names listed in the statement were not
found. Check that the names exist and are spelled correctly before rerunning the statement.

Log file name cannot be generated from a raw device. The log file name and path must be specified.

User does not have permission to alter database '%.*ls'.

The name of the primary filegroup cannot be changed.

The master and model databases cannot have files added to them. ALTER DATABASE was aborted.

The %S_MSG '%.*ls' does not exist in database '%.*ls'.

ALTER DATABASE failed. The total size specified must be 1 MB or greater.

System databases master, model, and tempdb cannot have their names changed.

ALTER DATABASE failed. Database '%.*ls' was not created with 'FOR LOAD'
option.

File '%.*ls' modified in sysaltfiles. Delete old file after restarting SQL Server.

Cannot find entry in sysaltfiles for file '%.*ls'.

The primary data or log file cannot be removed from a database.

The %S_MSG name '%.*ls' has been set.

Log file '%ls' for this database is already active.

Database must be put in bypass recovery mode to rebuild the log.

No entry found for the primary log file in sysfiles1. Could not rebuild the log.

The file '%ls' already exists. It should be renamed or deleted so that a new log file can be created.

Could not create a new log file with file '%.*ls'. See previous errors.

System databases master, model, and tempdb cannot have their logs rebuilt.

The system could not activate enough of the database to rebuild the log.

Warning: The log for database '%.*ls' has been rebuilt. Transactional consistency has been lost. DBCC CHECKDB should be run to validate physical consistency. Database options will have to be reset, and extra log files may need to be deleted.

Description

The database could not be exclusively locked to perform the operation.

Cannot remove the file '%.*ls' because it is the only file in the DEFAULT filegroup.

The file cannot be shrunk below page %ud until the log is backed up because it contains bulk logged pages.

Filegroup '%.*ls' already exists in this database.

MODIFY FILE failed. Specify logical name.

MODIFY FILE failed. Do not specify physical name.

MODIFY FILE failed for file "%.*ls". At least one property per file must be specified.

MODIFY FILE failed. Specified size is less than current size.

MODIFY FILE failed. Size is greater than MAXSIZE.

MODIFY FILE failed. File '%.*ls' does not exist.

The %S_MSG '%.*ls' cannot be removed because it is not empty.

The %S_MSG '%.*ls' cannot be found in %ls.

The %S_MSG '%.*ls' has been removed.

The %S_MSG already has the '%ls' property set.

The %S_MSG property '%ls' has been set.

Cannot change the READONLY property of the PRIMARY filegroup.

Cannot add, remove, or modify files in filegroup '%.*ls'. The filegroup is read-only.

Cannot extend file '%ls' using this syntax as it was not created with DISK INIT. Use ALTER DATABASE MODIFY FILE.

Cannot change the properties of empty filegroup '%.*ls'.The filegroup must contain at least one file.

Cannot have a filegroup with the name 'DEFAULT'.

The maximum of %ld filegroups per database has been exceeded.

Could not cleanup worktable IAM chains to allow shrink or remove file operation. Please try again when tempdb is idle.

Cannot add, remove, or modify file '%.*ls'. The file is read-only.

Cannot add, remove, or modify a file in filegroup '%.*ls' because the filegroup is offline.

Cannot add, remove, or modify file '%.*ls' because it is offline.

Option '%.*ls' cannot be set in database '%.*ls'.

Database '%.*ls' is in transition. Try the ALTER DATABASE statement later.

Nonqualified transactions are being rolled back. Estimated rollback completion:
%d%%.

ALTER DATABASE failed because a lock could not be placed on database '%.*ls'.
Try again later.

Description

Option '%.*ls' cannot be set at the same time as another option setting.

Database '%.*ls' is in warm standby. A warm-standby database is read-only.

Changes to the state or options of database '%.*ls' cannot be made at this time. The database is in single-user mode, and a user is currently connected to it.

Database '%.*ls' cannot be opened.

Database options single user and dbo use only cannot be set at the same time.

Failed to restart the current database. The current database is switched to master.

ALTER DATABASE statement failed.

Database state cannot be changed while other users are using the database '%.*ls'

ALTER DATABASE failed. The default collation of database '%.*ls' cannot be set
to %.*ls.

Cannot alter collation for database '%ls' because it is READONLY, OFFLINE, or marked SUSPECT.

The %S_MSG '%.*ls' is dependent on %S_MSG '%.*ls'.

The %S_MSG '%.*ls' is dependent on %S_MSG.

Warning: Changing default collation for database '%.*ls', which is used in replication. It is recommend that all replication database have the same default collation.

Table 3–44:TSQL Error Codes - 5100 to 5199

Description

You must supply parameters for the DISK %hs statement. Usage: %hs.

No such statement DISK %.*ls.

MAXSIZE cannot be less than SIZE for file '%ls'.

File '%.*ls' already used.

Device activation error. The physical file name '%.*ls' may be incorrect.

Parameter '%hs' requires value of data type '%hs'.

Value is wrong data type for parameter '%hs' (requires data type '%hs').

Log file '%.*ls' does not match the primary file. It may be from a different database or the log may have been rebuilt previously.

No such parameter '%.*ls'.

File '%.*ls' is on a network device not supported for database files.

You do not have permission to run DISK statements.

Could not run DISK statement. You must be in the master database to run this statement.

Each disk file size must be greater than or equal to 1 MB.

Description

CREATE FILE encountered operating system error %ls while attempting to open
or create the physical file '%.*ls'.

The logical device '%.*ls' does not exist in sysdevices.

The %hs of %d is out of range. It must be between %d and %d.

Could not set the file size to the desired amount. The operating system file size limit may have been reached.

MODIFY FILE encountered operating system error %ls while attempting to expand the physical file.

The size of a single log file must not be greater than 2 TB.

The %hs statement is obsolete and no longer supported.

I/O error encountered in the writelog system function during backout.

Warning: Media in device '%.*ls' may have been changed.

Operating system error %.*ls on device '%.*ls' during %ls.

Cannot take '%.*ls' offline because the database is in use.

Cannot find '%.*ls' in sysdatabases.

Cannot open '%.*ls' to take offline.

Usage: DBCC DBCONTROL(dbname,ONLINE|OFFLINE)

Cannot explicitly open or close master database.

Database '%.*ls' is already offline.

File '%.*ls' is on a network drive, which is not allowed.

FILEGROWTH cannot be greater than MAXSIZE for file '%.*ls'.

Cannot create file '%ls' because it already exists.

%.*ls is not a primary database file.

The header for file '%ls' is not a valid database file header. The %ls property is incorrect.

Cannot associate files with different databases.

Each file size must be greater than or equal to 512 KB.

The file '%.*ls' has been expanded to prevent recovery from failing. Contact the system administrator for further assistance.

The file '%.*ls' has been expanded beyond its maximum size to prevent recovery from failing. Contact the system administrator for further assistance.

Encountered an unexpected error while checking the sector size for file '%.*ls'. Check the SQL Server error log for more information.

Cannot use file '%.*ls' because it was originally formatted with sector size %d and is now on a device with sector size %d.

Cannot use file '%.*ls', which is on a device with sector size %d. SQL Server supports a maximum sector size of 4096 bytes.

Description

Could not open FCB for invalid file ID %d in database '%.*ls'.

Could not restart database '%.*ls'. Reverting back to old status.

New log file '%.*ls' was created.

File '%ls' cannot be created. Use WITH MOVE to specify a usable physical file name.

Cannot use file '%.*ls' for clustered server. Only formatted files on which the cluster resource of the server has a dependency can be used.

Table 3–45:TSQL Error Codes - 5700 to 5799

Description

Changed database context to '%.*ls'.

SQL Server is terminating this process.

Changed language setting to %.*ls.

Table 3–46:TSQL Error Codes - 5800 to 5899

Description

Unknown config number (%d) in sysconfigures.

Character set, sort order, or collation cannot be changed because at least one database is not writable.

Too few locks specified. Minimum %d.

Recovery intervals above %d minutes not recommended. Use the RECONFIGURE
WITH OVERRIDE statement to force this configuration.

Ad hoc updates to system catalogs not recommended. Use the RECONFIGURE
WITH OVERRIDE statement to force this configuration.

Average time slices above %d milliseconds not recommended. Use the RECONFIGURE WITH OVERRIDE statement to force this configuration.

Valid values for the fill factor are 0 to 100.

You do not have permission to run the RECONFIGURE statement.

Cannot reconfigure SQL Server to use sort order ID %d, because the row for that sort order does not exist in syscharsets.

User connections are limited to %d.

The specified user options value is invalid.

The default collation for SQL Server has been reconfigured. Restart SQL Server to rebuild the table indexes on columns of character data types.

Minimum server memory value (%d) must be less than or equal to the maximum value (%d).

Table 3–47:TSQL Error Codes - 5900 to 5999

Description

Background checkpoint process suspended until locks are available.

Table 3–48:TSQL Error Codes - 6000 to 6099

Description

SHUTDOWN is waiting for %d process(es) to complete.

SHUTDOWN is in progress. Log off.

User does not have permission to perform this action.

SHUTDOWN is in progress.

Server shut down by request.

The SHUTDOWN statement cannot be executed within a transaction or by a stored procedure.

Table 3–49:TSQL Error Codes - 6100 to 6199

Description

Process ID %d is not a valid process ID. Choose a number between 1 and %d.

User does not have permission to use the KILL statement.

Could not do cleanup for the killed process. Received message %d.

Cannot use KILL to kill your own process.

Process ID %d is not an active process ID.

Only user processes can be killed.

KILL SPID WITH COMMIT/ABORT is not supported by Microsoft SQL Server
2000. Use Microsoft Distributed Transaction Coordinator to resolve distributed
transactions.

SPID %d: transaction rollback in progress. Estimated rollback completion: %d%%. Estimated time remaining: %d seconds.

The distributed transaction with UOW %s does not exist.

Another user has decided a different outcome for the distributed transaction associated with UOW %s.

Distributed transaction with UOW %s is in prepared state. Only Microsoft Distributed Transaction Coordinator can resolve this transaction. KILL command failed.

The distributed transaction associated with UOW %s is in PREPARE state. Use KILL UOW WITH COMMIT/ABORT syntax to kill the transaction instead.

Distributed transaction with UOW %s is being used by another user. KILL command failed.

KILL command cannot be used inside user transactions.

Description

KILL command failed.

There is a connection associated with the distributed transaction with UOW %s. First, kill the connection using KILL SPID syntax.

The distributed transaction associated with UOW %s is not in PREPARED state. Use KILL UOW to kill the transaction instead.

Distributed transaction with UOW %s is rolling back: estimated rollback completion:
%d%%, estimated time left %d seconds.

Status report cannot be obtained. Rollback operation for Process ID %d is not in progress.

Status report cannot be obtained. Rollback operation for UOW %s is not in progress.

Table 3–50:TSQL Error Codes - 6400 to 6499

Description

Cannot roll back %.*ls. No transaction or savepoint of that name was found.

Table 3–51:TSQL Error Codes - 6600 to 6799

Description

XML error: %.*ls

XML parser returned the error code %d from line number %d, source '%.*ls'.

The error description is '%.*ls'.

XML parsing error: %.*ls

XML stored procedures are not supported in fibers mode.

%.*ls: Failed to obtain an IPersistStream interface on the XML text.

%.*ls: Failed to save the XML text stream. The server resources may be too low.

%.*ls: The value supplied for parameter number %d is invalid.

Failed to instantiate class '%ls'. Make sure Msxml2.dll exists in the SQL Server installation.

Column '%ls' contains an invalid data type. Valid data types are char, varchar, nchar, nvarchar, text, and ntext.

Failed to load Msxml2.dll.

Invalid data type for the column indicated by the parameter '%ls'. Valid data types are int, bigint, smallint, and tinyint.

Specified value '%ls' already exists.

Value specified for column '%ls' is the same for column '%ls'. An element cannot be its own parent.

Invalid data type is specified for column '%ls'. Valid data types are int, bigint, smallint, and tinyint.

Description

Parameter '%ls' is required when the parent of the element to be added is missing and must be inserted.

The specified edge table has an invalid format. Column '%ls' is missing or has an invalid data type.

Column '%ls' in the specified edge table has an invalid or null value.

XML node of type %d named '%ls' cannot be created .

XML attribute or element cannot be created for column '%ls'.

XML encoding or decoding error occurred with object name '%.*ls'.

Invalid data type for column '%ls'. Data type cannot be text, ntext, image, or binary.

Column '%ls' contains an invalid data type. Valid data types are char, varchar, nchar, and nvarchar.

XML document could not be created because server memory is low. Use sp_xml_removedocument to release XML documents.

Table 3–52:TSQL Error Codes - 6800 to 6899

Description

FOR XML AUTO requires at least one table for generating XML tags. Use FOR XML RAW or add a FROM clause with a table name.

FOR XML EXPLICIT requires at least three columns, including the tag column, the parent column, and at least one data column.

FOR XML EXPLICIT query contains the invalid column name '%.*ls'. Use the TAGNAME!TAGID!ATTRIBUTENAME[!..] format where TAGID is a positive integer.

FOR XML EXPLICIT requires the first column to hold positive integers that represent XML tag IDs.

FOR XML EXPLICIT requires the second column to hold NULL or nonnegative integers that represent XML parent tag IDs.

FOR XML EXPLICIT stack overflow occurred. Circular parent tag relationships are not allowed.

Undeclared tag ID %d is used in a FOR XML EXPLICIT query.

Undeclared parent tag ID %d is used in a FOR XML EXPLICIT query.

XML tag ID %d could not be added. The server memory resources may be low.

Unnamed column or table names cannot be used as XML identifiers. Name unnamed columns using AS in the SELECT statement.

Column name '%.*ls' is repeated. The same attribute cannot be generated more than once on the same XML tag.

FOR XML is incompatible with COMPUTE expressions. Remove the COMPUTE expression.

XML tag ID %d that was originally declared as '%.*ls' is being redeclared as '%.*ls'.

Description

FOR XML EXPLICIT cannot combine multiple occurrences of ID, IDREF, IDREFS, NMTOKEN, and/or NMTOKENS in column name '%.*ls'.

In the FOR XML EXPLICIT clause, ID, IDREF, IDREFS, NMTOKEN, and NMTOKENS require attribute names in '%.*ls'.

In the FOR XML EXPLICIT clause, ID, IDREF, IDREFS, NMTOKEN, and NMTOKENS attributes cannot be hidden in '%.*ls'.

In the FOR XML EXPLICIT clause, ID, IDREF, IDREFS, NMTOKEN, and NMTOKENS attributes cannot be generated as CDATA, XML, or XMLTEXT in '%.*ls'.

FOR XML EXPLICIT cannot combine multiple occurrences of ELEMENT, XML, XMLTEXT, and CDATA in column name '%.*ls'.

In the FOR XML EXPLICIT clause, CDATA attributes must be unnamed in '%.*ls'.

The FOR XML clause is not allowed in a %ls statement.

FOR XML EXPLICIT requires column %d to be named '%ls' instead of '%.*ls'.

GROUP BY and aggregate functions are currently not supported with FOR XML
AUTO.

In the FOR XML EXPLICIT clause, mode '%.*ls' in a column name is invalid.

ELEMENTS mode requires FOR XML AUTO.

Every IDREFS or NMTOKENS column in a FOR XML EXPLICIT query must appear in a separate SELECT clause, and the instances must be ordered directly after the element to which they belong.

FOR XML EXPLICIT queries allow only one XMLTEXT column per tag. Column '%.*ls' declares another XMLTEXT column that is not permitted.

XMLTEXT column '%.*ls' must be of a string data type.

FOR XML EXPLICIT and RAW modes currently do not support addressing binary data as URLs in column '%.*ls'. Remove the column, or use the BINARY BASE64 mode, or create the URL directly using the
'dbobject/TABLE[@PK1="V1"]/@COLUMN' syntax.

FOR XML AUTO could not find the table owning the following column '%.*ls' to create a URL address for it. Remove the column, or use the BINARY BASE64 mode, or create the URL directly using the
'dbobject/TABLE[@PK1="V1"]/@COLUMN' syntax.

FOR XML AUTO requires primary keys to create references for '%.*ls'. Select primary keys, or use BINARY BASE64 to obtain binary data in encoded form if no primary keys exist.

FOR XML AUTO cannot generate a URL address for binary data if a primary key is also binary.

Parent tag ID %d is not among the open tags. FOR XML EXPLICIT requires parent tags to be opened first. Check the ordering of the result set.

XMLTEXT field '%.*ls' contains an invalid XML document. Check the root tag and its attributes.

Description

FOR XML EXPLICIT field '%.*ls' can specify the directive HIDE only once.

FOR XML EXPLICIT requires attribute-centric IDREFS or NMTOKENS field '%.*ls' to precede element-centric IDREFS/NMTOKEN fields.

The XMLTEXT document attribute that starts with '%.*ls' is too long. Maximum length is %d.

Attribute-centric IDREFS or NMTOKENS field not supported on tags having element-centric field '%.*ls' of type TEXT/NTEXT or IMAGE. Either specify ELEMENT on IDREFS/NMTOKENS field or remove the ELEMENT directive.

FOR XML EXPLICIT does not support XMLTEXT field on tag '%.*ls' that has IDREFS or NMTOKENS fields.

XMLDATA does not support namespace elements or attributes such as '%.*ls'. Run the SELECT FOR XML statement without XMLDATA or remove the namespace prefix declaration.

Table 3–53:TSQL Error Codes - 7000 to 7099

Description

OPENXML document handle parameter must be of data type int.

OPENXML flags parameter must be of data type int.

OPENXML XPath must be of a string data type, such as nvarchar.

Only one OPENXML column can be of type %ls.

OPENXML does not support retrieving schema from remote tables, as in '%.*ls'.

OPENXML requires a metaproperty namespace to be declared if 'mp' is used for another namespace in sp_xml_preparedocument.

OPENXML encountered a problem identifying the metaproperty namespace prefix. Consider removing the namespace parameter from the corresponding sp_xml_preparedocument statement.

OPENXML encountered unknown metaproperty '%.*ls'.

The OPENXML EDGETABLE is incompatible with the XMLTEXT OVERFLOW flag.

OPENXML allows only one metaproperty namespace prefix declaration in sp_xml_preparedocument.

Table 3–54:TSQL Error Codes - 7100 to 7199

Description

You cannot use a text pointer for a table with option 'text in row' set to ON.

SQL Server Internal Error. Text manager cannot continue with current statement.

You cannot set option 'text in row' for table %s.

Offset or size type is invalid. Must be int or smallint data type.

Description

Page %S_PGID, slot %d for text, ntext, or image node does not exist.

You cannot update a blob with a read-only text pointer

You can have only 1,024 in-row text pointers in one transaction

Offset %d is not in the range of available text, ntext, or image data.

Invalid text, ntext, or image pointer type. Must be binary(16).

Invalid text, ntext, or image pointer value %hs.

The offset and length specified in the READTEXT statement is greater than the actual data length of %ld.

The text, ntext, or image pointer value conflicts with the column name specified.

The text, ntext, or image pointer value references a data page with an invalid text, ntext, or image status.

The text, ntext, or image pointer value references a data page with an invalid timestamp.

The text, ntext, or image pointer value references a data page that is no longer allocated.

%ls WITH NO LOG is not valid at this time. Use sp_dboption to set the 'select into/bulkcopy' option on for database '%.*ls'.

NULL textptr (text, ntext, or image pointer) passed to %ls function.

Deletion length %ld is not in the range of available text, ntext, or image data.

%s is not allowed because the column is being processed by a concurrent snapshot and is being replicated to a non-SQL Server Subscriber or Published in a publication allowing Data Transformation Services (DTS).

The WRITETEXT statement is not allowed because the column is being replicated with Data Transformation Services (DTS).

Length of text, ntext, or image data (%ld) to be replicated exceeds configured maximum %ld.

Must create orphaned text inside a user transaction.

Must drop orphaned text before committing the transaction.

Invalid locator de-referenced.

Table 3–55:TSQL Error Codes - 7200 to 7299

Description

Could not execute procedure on remote server '%.*ls' because SQL Server is not configured for remote access. Ask your system administrator to reconfigure SQL Server to allow remote access.

Could not find server '%.*ls' in sysservers. Execute sp_addlinkedserver to add the server to sysservers.

Could not execute procedure '%.*ls' on remote server '%.*ls'.

Could not set up parameter for remote server '%.*ls'.

Remote procedure time out of %d seconds exceeded. Remote procedure '%.*ls' is canceled.

Could not relay results of procedure '%.*ls' from remote server '%.*ls'.

OLE DB error trace [%ls].

Table 3–56:TSQL Error Codes - 7300 to 7399

Description

Could not obtain a required interface from OLE DB provider '%ls'.

Could not create an instance of OLE DB provider '%ls'.

Could not initialize data source object of OLE DB provider '%ls'. %ls

Could not create a new session on OLE DB provider '%ls'.

Could not create a statement object using OLE DB provider '%ls'.

Could not open table '%ls' from OLE DB provider '%ls'. %ls

Could not obtain the data source of a session from OLE DB provider '%ls'. This action must be supported by the provider.

Could not obtain the schema options for OLE DB provider '%ls'. The provider supports the interface, but returns a failure code when it is used.

Could not obtain the schema rowset for OLE DB provider '%ls'. The provider supports the interface, but returns a failure code when it is used.

Invalid use of schema and/or catalog for OLE DB provider '%ls'. A four-part name was supplied, but the provider does not expose the necessary interfaces to use a catalog and/or schema.

Invalid schema or catalog specified for provider '%ls'.

OLE DB provider '%ls' does not contain table '%ls'. The table either does not exist or the current user does not have permissions on that table.

OLE DB provider '%ls' contains multiple tables that match the name '%ls'.

Could not use qualified table names (schema or catalog) with OLE DB provider '%ls' because it does not implement required functionality.

OLE DB provider '%ls' returned an invalid schema definition.

Description

OLE DB provider '%ls' returned an invalid column definition.

OLE DB provider '%ls' returned a '%ls' index '%ls' with incorrect bookmark ordinal %d.

Could not execute query against OLE DB provider '%ls'. %ls

An error occurred while preparing a query for execution against OLE DB provider '%ls'. %ls

A failure occurred while giving parameter information to OLE DB provider '%ls'. %ls

An error occurred while submitting the query text to OLE DB provider '%ls'. %ls

Could not fetch a row from OLE DB provider '%ls'. %ls

Rows from OLE DB provider '%ls' cannot be released. %ls

Could not rescan the result set from OLE DB provider '%ls'. %ls

Could not fetch a row using a bookmark from OLE DB provider '%ls'. %ls

Could not create a column accessor for OLE DB provider '%ls'. %ls

Could not get the current row value of column '%ls.%ls' from the OLE DB provider '%ls'. %ls

Unexpected NULL value returned for column '%ls.%ls' from the OLE DB provider '%ls'. This column cannot be NULL.

OLE DB provider '%ls' could not %ls table '%ls'. %ls

OLE DB provider '%ls' could not %ls table '%ls' because of column '%ls'. %ls

OLE DB provider '%ls' could not delete from table '%ls'. %ls

Could not get the data of the row from the OLE DB provider '%ls'. %ls

OLE DB provider '%ls' returned an unexpected data length for the fixed-length column '%ls.%ls'. The expected data length is %ls, while the returned data length is %ls.

OLE DB provider '%ls' could not set range for table '%ls'.%ls

OLE DB provider '%ls' could not set range for table '%ls' because of column '%ls'.%ls

Could not get the column information from the OLE DB provider '%ls'.

OLE DB provider '%ls' could not map ordinals for one or more columns of object '%ls'.

OLE DB provider '%ls' supplied inconsistent metadata. The object '%ls' was missing expected column '%ls'.

OLE DB provider '%ls' supplied inconsistent metadata. An extra column was supplied during execution that was not found at compile time.

OLE DB provider '%ls' supplied invalid metadata for column '%ls'. %ls

OLE DB provider '%ls' supplied inconsistent metadata for a column. The name was changed at execution time.

Description

OLE DB provider '%ls' supplied inconsistent metadata for a column. Metadata information was changed at execution time.

Could not process object '%ls'. The OLE DB provider '%ls' indicates that the object has no columns.

Could not execute query.The OLE DB provider '%ls' did not provide an appropriate interface to access the text, ntext, or image column '%ls.%ls'.

The OLE DB provider '%ls' reported a schema version for table '%ls' that changed between compilation and execution.

Could not get the length of a storage object from the OLE DB provider '%ls' for table '%ls', column '%ls'.

Could not read a storage object from the OLE DB provider '%ls', for table '%ls', column '%ls'.

The OLE DB provider '%ls' reported different meta data at for table '%ls' column '%ls'.

Could not obtain optional metadata columns of columns rowset from the OLE DB provider '%ls'.

Could not obtain columns rowset from OLE DB provider '%ls'. The provider supports the interface, but returns a failure code when used.

The OLE DB provider '%ls' supports column-level collation, but failed to provide metadata column '%ls' at .

The OLE DB provider '%ls' supports column-level collation, but failed to provide collation data for column '%ls'.

The OLE DB provider '%ls' provided invalid collation. %ls.

One or more properties could not be set on the query for OLE DB provider '%ls'. %ls

One or more properties could not be set on the table for OLE DB provider '%ls'.

Cannot get properties from OLE DB provider '%ls'.

Could not set the initialization properties for the OLE DB provider '%ls'.

Could not set the session properties for the OLE DB provider '%ls'.

Could not open index '%ls' on table '%ls' from OLE DB provider '%ls'. %ls

Could not enforce the remote join hint for this query.

Cannot specify an index or locking hint for a remote data source.

The update/delete operation requires a unique key or a clustered index on the remote table.

OLE DB provider '%ls' returned an unexpected '%ls' for the decimal/numeric column '%ls.%ls'. The expected data length is '%ls', while the returned data length is '%ls'.

The requested operation could not be performed because the OLE DB provider '%ls' does not support the required transaction interface.

Description

The operation could not be performed because the OLE DB provider '%ls' was unable to begin a distributed transaction.

Could not start a transaction for OLE DB provider '%ls'.

OLE DB provider '%ls' reported an error aborting the current transaction.

OLE DB provider '%ls' reported an error committing the current transaction.

Unable to start a nested transaction for OLE DB provider '%ls'. A nested transaction was required because the XACT_ABORT option was set to OFF.

OLE DB provider '%ls' reported an error. %ls

Table 3–57:TSQL Error Codes - 7400 to 7499

Description

Cannot create OLE DB provider enumeration object installed with SQL Server. Verify installation.

Could not locate registry entry for OLE DB provider '%ls'.

The server could not load DCOM.

Heterogeneous queries require the ANSI_NULLS and ANSI_WARNINGS options to be set for the connection. This ensures consistent query semantics. Enable these options and then reissue your query.

Remote access not allowed for Windows NT user activated by SETUSER.

Server '%.*ls' is not configured for %ls.

Could not perform a Windows NT authenticated login because delegation is not available.

Invalid number of parameters. Rowset '%ls' expects %d parameter(s).

Ad hoc access to OLE DB provider '%ls' has been denied. You must access this provider through a linked server.

Access to the remote server is denied because no login-mapping exists.

GROUP BY ALL is not supported in queries that access remote tables if there is also a WHERE clause in the query.

Text, image, or ntext column was too large to send to the remote data source due to the storage interface used by the provider.

Lazy schema validation error. Linked server schema version has changed. Re-run the query.

Table 3–58:TSQL Error Codes - 7600 to 7699

Description

Cannot use a CONTAINS or FREETEXT predicate on %S_MSG '%.*ls' because it is not full-text indexed.

The Full-Text Service (Microsoft Search) is not available. The system administrator must start this service.

Description

Syntax error in search condition, or empty or null search condition '%ls'.

Full-text operation failed due to a time out.

Full-text catalog '%ls' has been lost. Use sp_fulltext_catalog to rebuild and to repopulate this full-text catalog.

Could not find full-text index for database ID %d, table ID %d. Use sp_fulltext_table to deactivate then activate this index.

Search on full-text catalog '%ls' for database ID %d, table ID %d with search condition '%ls' failed with unknown result (%x).

An unknown full-text failure (%x) occurred in function %hs on full-text catalog '%ls'.

Full-Text Search is not installed, or a full-text component cannot be loaded.

Access is denied to '%ls', or the path is invalid. Full-text search was not installed properly.

Warning: Request to start a population in full-text catalog '%ls' ignored because a population is currently active for this full-text catalog.

%d is not a valid value for full-text system resource usage.

Cannot drop index '%.*ls' because it enforces the full-text key for table '%.*ls'.

Cannot alter or drop column '%.*ls' because it is enabled for Full-Text Search.

A CONTAINS or FREETEXT predicate can only operate on one table. Qualify the use of * with a table name.

Full-Text Search is not enabled for the current database. Use sp_fulltext_database to enable full-text search for the database.

Query does not reference the full-text indexed table.

%d is not a valid value for a full-text connection time out.

Conversion to data type %ls failed for full-text search key value 0x%ls.

Invalid use of full-text predicate in the HAVING clause.

Full-text catalog '%ls' lacks sufficient disk space to complete this operation.

Full-text query failed because full-text catalog '%ls' is not yet ready for queries.

Full-text catalog '%ls' is in a unusable state. Drop and re-create this full-text catalog.

Full-text table has more than one LCID among its full-text indexed columns.

The top_n_by_rank argument ('%d') must be greater than zero.

Full-text catalog in directory '%ls' for clustered server cannot be created. Only directories on a disk in the cluster group of the server can be used.

Cannot copy Schema.txt to '%.*ls' because access is denied or the path is invalid. Full-text search was not installed properly.

Cannot open or query registry key '%.*ls'.

Description

Syntax error occurred near '%.*ls' in search condition '%.*ls'.

Syntax error occurred near '%.*ls'. Expected '%.*ls' in search condition '%.*ls'.

The value of the Weight argument must be between 0.0 and 1.0.

The syntax <content search condition> OR NOT <content boolean term> is not allowed.

Stack overflow occurred in parsing search condition '%.*ls'.

The Microsoft Search service cannot be administered under the present user account

Warning: Request to start a full-text index population on table '%ls' is ignored because a population is currently active for this table.

Value %d is not valid for full-text data time-out.

Warning: Request to stop change tracking has deleted all changes tracked on table '%ls'.

Cannot use a full-text predicate on %S_MSG '%.*ls' because it is not located on the local server.

Warning: Request to stop tracking changes on table '%ls' will not stop population currently in progress on the table.

Full-Text catalog '%ls' does not exist.

A full-text catalog named '%ls' already exists in this database.

Table 3–59:TSQL Error Codes - 7900 to 7999

Description

The object specified is neither a table nor a constraint

The table '%.*ls' was created with the NO_LOG option.

Repair: Page %S_PGID has been allocated to object ID %d, index ID %d.

Repair: Page %S_PGID has been deallocated from object ID %d, index ID %d.

Repair: Extent %S_PGID has been allocated to object ID %d, index ID %d.

Repair: Extent %S_PGID has been deallocated from object ID %d, index ID %d.

Repair: %ls page at %S_PGID has been rebuilt.

Repair: IAM chain for object ID %d, index ID %d, has been truncated before page %S_PGID and will be rebuilt.

Repair: Deleted record for object ID %d, index ID %d, on page %S_PGID, slot %d. Indexes will be rebuilt.

Repair: Converted forwarded record for object ID %d, index ID %d, at page %S_PGID, slot %d to a data row.

Repair: Page %S_PGID next and %S_PGID previous pointers have been set to match each other in object ID %d, index ID %d.

Repair statement not processed. Database needs to be in single user mode.

Description

Processed %ld entries in sysindexes for database ID %d.

***************************************************************

Table %.*ls Object ID %ld.

Index ID %ld. FirstIAM %S_PGID. Root %S_PGID. Dpages %ld.

Index ID %d. %ld pages used in %ld dedicated extents.

Total number of extents is %ld.

The indexes for '%.*ls' are already correct. They will not be rebuilt.

One or more indexes contain errors. They will be rebuilt.

The table '%.*ls' has no indexes.

REINDEX received an exception. Statement terminated.

The data in table '%.*ls' is possibly inconsistent. REINDEX terminated. Run DBCC CHECKTABLE and report errors to your system administrator.

Cannot detach database '%.*ls' because it does not exist.

System databases master, model, msdb, and tempdb cannot be detached.

Trace option(s) not enabled for this connection. Use 'DBCC TRACEON()'.

DBCC %ls scanning '%.*ls' table...

Table: '%.*ls' (%d); index ID: %d, database ID: %d

%ls level scan performed.

Invalid SPID %d specified.

Permission to execute DBCC %ls denied.

Cannot display the specified SPID's buffer; in transition.

The specified SPID does not process input/output data streams.

The DBCC statement is not supported in this release.

Description

Object ID %d, index ID %d, page ID %S_PGID, row ID %d. Column '%.*ls' is a var column with a NULL value and non-zero data length.

Upgrade requires SQL Server to be started in single user mode. Restart SQL Server with the -m flag.

Upgrade encountered a fatal error. See the SQL Server errorlog for more information.

Table error: Could not check object ID %d, index ID %d due to invalid allocation (IAM) page(s).

Warning: NO_INDEX option of %ls being used. Checks on non-system indexes will be skipped.

Transaction information for database '%.*ls'.

No active open transactions.

%hsOldest active transaction:

SPID (server process ID) : %d

UID (user ID) : %d

Name : %.*ls

LSN : (%d:%d:%d)

Start time : %.*ls

%hsReplicated Transaction Information:

Oldest distributed LSN : (%d:%d:%d)

Oldest non-distributed LSN : (%d:%d:%d)

User '%.*ls' does not have permission to run DBCC %ls for database '%.*ls'.

Invalid object name '%.*ls'.

The object name '%.*ls' contains more than the maximum number of prefixes. The maximum is %d.

Warning: Pinning tables should be carefully considered. If a pinned table is larger, or grows larger, than the available data cache, the server may need to be restarted and the table unpinned.

System table mismatch: Table '%.*ls', object ID %d has index ID 1 in sysindexes but the status in sysobjects does not have the clustered bit set. The table will be checked as a heap.

Cannot shrink 'read only' database '%.*ls'.

Cannot shrink file '%d' in database '%.*ls' to %d pages as it only contains %d pages.

Object ID %d, index ID %d: FirstIAM field in sysindexes is %S_PGID. FirstIAM for statistics only and dummy index entries should be (0:0).

Database '%ls' consistency errors in sysobjects, sysindexes, syscolumns, or systypes prevent further %ls processing.

Description

Extended stored procedures can only be created in the master database.

'%.*ls' does not contain an identity column.

Checking identity information: current identity value '%.*hs', current column value '%.*hs'.

Could not find any index named '%.*ls' for table '%.*ls'.

Table 3–60:TSQL Error Codes - 8100 to 8199

Description

An explicit value for the identity column in table '%.*ls' can only be specified when a column list is used and IDENTITY_INSERT is ON.

Cannot update identity column '%.*ls'.

Table '%.*ls' does not exist or cannot be opened for SET operation.

The current user is not the database or object owner of table '%.*ls'. Cannot perform SET operation.

'%.*ls' is not a user table. Cannot perform SET operation.

Table '%.*ls' does not have the identity property. Cannot perform SET operation.

IDENTITY_INSERT is already ON for table '%.*ls.%.*ls.%.*ls'. Cannot perform SET operation for table '%.*ls'.

Cannot add identity column, using the SELECT INTO statement, to table '%.*ls', which already has column '%.*ls' that inherits the identity property.

Attempting to add multiple identity columns to table '%.*ls' using the SELECT INTO statement.

Cannot add multiple PRIMARY KEY constraints to table '%.*ls'.

Cannot define PRIMARY KEY constraint on nullable column in table '%.*ls'.

Cannot add more than one clustered index for constraints on table '%.*ls'.

Error converting data type %ls to %ls.

Arithmetic overflow error converting %ls to data type %ls.

Argument data type %ls is invalid for argument %d of %ls function.

Operand data type %ls is invalid for %ls operator.

Column '%.*ls.%.*ls' is invalid in the select list because it is not contained in an aggregate function and there is no GROUP BY clause.

Column '%.*ls.%.*ls' is invalid in the HAVING clause because it is not contained in an aggregate function and there is no GROUP BY clause.

Column '%.*ls.%.*ls' is invalid in the select list because it is not contained in either an aggregate function or the GROUP BY clause.

Column '%.*ls.%.*ls' is invalid in the HAVING clause because it is not contained in either an aggregate function or the GROUP BY clause.

Only the first query in a UNION statement can have a SELECT with an assignment.

Description

A correlated expression is invalid because it is not in a GROUP BY clause.

Multiple columns are specified in an aggregated expression containing an outer reference. If an expression being aggregated contains an outer reference, then that outer reference must be the only column referenced in the expression.

An aggregated expression containing an outer reference must be contained in either the select list, or a HAVING clause subquery in the query whose FROM clause contains the table with the column being aggregated.

Column name '%.*ls.%.*ls' is invalid in the ORDER BY clause because it is not contained in an aggregate function and there is no GROUP BY clause.

Column name '%.*ls.%.*ls' is invalid in the ORDER BY clause because it is not contained in either an aggregate function or the GROUP BY clause.

Using '%s' version '%s' to execute extended stored procedure '%s'.

The new disk size must be greater than %d. Consider using DBCC SHRINKDB.

The device is not a database device. Only database devices can be expanded.

Extended stored procedure DLL '%s' does not export __GetXpVersion(). Refer to the topic "Backward Compatibility Details (Level 1) - Open Data Services" in the documentation for more information.

Extended stored procedure DLL '%s' reports its version is %d.%d. Server expects version %d.%d.

None of the result expressions in a CASE specification can be NULL.

Divide by zero error encountered.

Table level constraint does not specify column list, table '%.*ls'.

Duplicate columns specified in %ls constraint key list, table '%.*ls'.

More than 16 columns specified in foreign key column list, table '%.*ls'.

Number of referencing columns in foreign key differs from number of referenced columns, table '%.*ls'.

More than one key specified in column level %ls constraint, table '%.*ls'.

Column %ls constraint for column '%.*ls' references another column, table '%.*ls'.

Subqueries are not supported in %ls constraints, table '%.*ls'.

Parameter '%.*ls' was supplied multiple times.

Procedure or function %.*ls has too many arguments specified.

%.*ls is not a parameter for procedure %.*ls.

Procedure %.*ls has no parameters and arguments were supplied.

Could not create IDENTITY attribute on nullable column '%.*ls', table '%.*ls'.

More than one column %ls constraint specified for column '%.*ls', table '%.*ls'.

OLE Automation objects are not supported in fiber mode.

Multiple NULL constraints were specified for column '%.*ls', table '%.*ls'.

Description

Both a PRIMARY KEY and UNIQUE constraint have been defined for column '%.*ls', table '%.*ls'. Only one is allowed.

String or binary data would be truncated.

Warning: Null value is eliminated by an aggregate or other SET operation.

The table '%.*ls' is ambiguous.

No column was specified for column %d of '%.*ls'.

The column '%.*ls' was specified multiple times for '%.*ls'.

All the queries in a query expression containing a UNION operator must have the same number of expressions in their select lists.

'%.*ls' has more columns than were specified in the column list.

'%.*ls' has fewer columns than were specified in the column list.

A grouping function can only be specified when either CUBE or ROLLUP is specified in the GROUP BY clause.

A grouping function argument does not match any of the expressions in the GROUP BY clause.

Formal parameter '%.*ls' was defined as OUTPUT but the actual parameter not declared OUTPUT.

The text, ntext, or image data type cannot be selected as DISTINCT.

An INSERT EXEC statement cannot be nested.

Invalid subcommand value %d. Legal range from %d to %d.

Constraint name '%.*ls' not permitted. Constraint names cannot begin with a number sign (#).

Cannot create two constraints named '%.*ls'. Duplicate constraint names are not allowed.

Syntax error converting from a character string to uniqueidentifier.

Insufficient result space to convert uniqueidentifier value to char.

Hint '%ls' on object '%.*ls' is invalid.

Could not find table %.*ls. Will try to resolve this table name later.

Resync procedure expects value of key '%.*ls', which was not supplied.

Cannot use a column in the %hs clause unless it is contained in either an aggregate function or the GROUP BY clause.

Prepared statement '%.*ls' expects parameter %.*ls, which was not supplied.

Could not find prepared statement with handle %d.

Statement(s) could not be prepared.

Text for '%.*ls' is missing from syscomments. The object must be dropped and re-created before it can be used.

Only UNIQUE or PRIMARY KEY constraints are allowed on computed columns.

Description

Error expanding '*': all columns incomparable, '*' expanded to zero columns.

Error expanding '*': An uncomparable column has been found in an underlying table or view.

Function '%.*ls' can be used only on user and system tables.

Cannot compile replication filter procedure without defining table being filtered.

Replication filter procedures can only contain SELECT, GOTO, IF, WHILE, RETURN, and DECLARE statements.

Replication filter procedures cannot have parameters.

Cannot execute a procedure marked FOR REPLICATION.

Cannot execute a USE statement while an application role is active.

Duplicate column specified as ROWGUIDCOL.

Windows NT user '%.*ls' does not have server access.

Could not obtain information about Windows NT group/user '%ls'.

In EXECUTE <procname>, procname can only be a literal or variable of type char, varchar, nchar, or nvarchar.

Table 3–61:TSQL Error Codes - 8500 to 8599

Description

MSDTC on server '%.*ls' is unavailable.

Unknown MSDTC token '0x%x' received.

Invalid transaction import buffer.

Invalid transaction state change requested from %hs to %hs.

QueryInterface failed for '%hs': %hs.

Import of MSDTC transaction failed: %hs.

Enlist of MSDTC transaction failed: %hs.

Unknown isolation level %d requested from MSDTC.

MSDTC Commit acknowledgment failed: %hs.

MSDTC Abort acknowledgment failed: %hs.

MSDTC PREPARE acknowledgment failed: %hs.

MSDTC Global state is invalid.

Failed to get MSDTC PREPARE information: %hs.

MSDTC BEGIN TRANSACTION failed: %hs.

Current MSDTC transaction must be committed by remote client.

Commit of internal MSDTC transaction failed: %hs.

Invalid awakening state. Slept in %hs; awoke in %hs.

Description

Distributed transaction aborted by MSDTC.

PREPARE TRAN statement not allowed on MSDTC transaction.

The current transaction could not be exported to the remote provider. It has been rolled back.

Distributed transaction completed. Either enlist this session in a new transaction or the NULL transaction.

Table 3–62:TSQL Error Codes - 8600 to 8699

Description

Internal Query Processor Error: The query processor could not obtain access to a required interface.

Indexes used in hints must be explicitly included by the index tuning wizard.

The index hints for table '%.*ls' were ignored because the table was considered a fact table in the star join.

Invalid Query: CUBE and ROLLUP cannot compute distinct aggregates.

Warning: The query processor could not produce a query plan from the optimizer because the total length of all the columns in the GROUP BY or ORDER BY clause exceeds 8000 bytes.

Warning: The query processor could not produce a query plan from the optimizer because the total length of all the columns in the GROUP BY or ORDER BY clause exceeds 8000 bytes. Resubmit your query without the ROBUST PLAN hint.

Internal Query Processor Error: The query processor encountered an internal limit overflow.

Internal Query Processor Error: The query processor ran out of stack space during query optimization.

Query processor could not produce a query plan because of the hints defined in
this query. Resubmit the query without specifying any hints and without using
SET FORCEPLAN.

Internal Query Processor Error: The query processor could not produce a query plan. Contact your primary support provider for more information.

Internal SQL Server error.

Warning: The join order has been enforced because a local join hint is used.

Only text pointers are allowed in work tables, never text, ntext, or image columns. The query processor produced a query plan that required a text, ntext, or image column in a work table.

The query processor could not produce a query plan because of the combination of hints and text, ntext, or image data passing through operators using work tables.

A time out occurred while waiting to optimize the query. Rerun the query.

Description

The query processor could not produce a query plan from the optimizer because a query cannot update a text, ntext, or image column and a clustering key at the same time.

Internal Query Processor Error: The query processor encountered an unexpected error during execution.

Internal Query Processor Error: The query processor encountered an unexpected work table error during execution.

The query processor could not start the necessary thread resources for parallel query execution.

Internal Query Processor Error: The plan selected for execution does not support the invoked given execution routine.

A time out occurred while waiting for memory resources to execute the query. Rerun the query.

The index entry for row ID %.*hs was not found in index ID %d, of table %d, in database '%.*ls'.

Scan on sysindexes for database ID %d, object ID %ld, returned a duplicate index ID %d. Run DBCC CHECKTABLE on sysindexes.

Could not insert a row larger than the page size into a hash table. Resubmit the query with the ROBUST PLAN hint.

The query has been canceled because the estimated cost of this query (%d) exceeds the configured threshold of %d. Contact the system administrator.

Intra-query parallelism caused your server command (process ID #%d) to deadlock. Rerun the query without intra-query parallelism by using the query hint option (maxdop 1).

Could not perform the requested operation because the minimum query memory is not available. Decrease the configured value for the 'min memory per query' server configuration option.

Warning: The query processor is unable to produce a plan because the table '%.*ls' is marked OFFLINE.

A cursor plan could not be generated for the given statement because it contains textptr ( inrow lob ).

An index cannot be created on the view '%.*ls' because the view definition does not include all the columns in the GROUP BY clause.

A clustered index cannot be created on the view '%.*ls' because the index key includes columns which are not in the GROUP BY clause.

An index cannot be created on the view '%.*ls' because the view definition includes an unknown value (the sum of a nullable expression).

An index cannot be created on the view '%.*ls' because the view definition does not include count_big(*).

An index cannot be created on the view '%.*ls' because the view definition includes duplicate column names.

Description

An index cannot be created on the view '%.*ls' because no row can satisfy the view definition.

Warning: The optimizer cannot use the index because the select list of the view contains a non-aggregate expression.

Warning: The optimizer cannot use the index because the group-by list in the view forms a key and is redundant.

Internal Query Processor Error: The query processor encountered an unexpected error during the processing of a remote query phase.

Table 3–63:TSQL Error Codes - 8900 to 8999

Description

Deadlock detected during DBCC. Complete the transaction in progress and retry this statement.

Memory allocation error during DBCC processing.

Extent %S_PGID in database ID %d is allocated in both GAM %S_PGID and
SGAM %S_PGID.

Extent %S_PGID in database ID %d is allocated by more than one allocation object.

Extent %S_PGID in database ID %d is marked allocated in the GAM, but no SGAM or IAM has allocated it.

Page %S_PGID in database ID %d is allocated in the SGAM %S_PGID and PFS %S_PGID, but was not allocated in any IAM. PFS flags '%hs'.

Table error: Database ID %d, object ID %d, index ID %d. Chain linkage mismatch. %S_PGID->next = %S_PGID, but %S_PGID->prev = %S_PGID.

Table error: Object ID %d, index ID %d, page ID %S_PGID. The PageId in the page header = %S_PGID.

Page %S_PGID in database ID %d is allocated to both object ID %d, index ID %d, and object ID %d, index ID %d.

The error has been repaired.

%.*ls fixed %d allocation errors and %d consistency errors in database '%ls'.

Extent %S_PGID is allocated to '%ls' and at least one other object.

Incorrect PFS free space information for page %S_PGID, object ID %d, index ID %d, in database ID %d. Expected value %hs, actual value %hs.

File %d (number of mixed extents = %ld, mixed pages = %ld).

Object ID %ld, Index ID %ld, data extents %ld, pages %ld, mixed extent pages %ld.

Object ID %ld, Index ID %ld, index extents %ld, pages %ld, mixed extent pages %ld.

(number of mixed extents = %ld, mixed pages = %ld) in this database.

Description

Single page allocation %S_PGID in table %ls, object ID %d, index ID %d is not allocated in PFS page ID %S_PGID.

Cannot perform a %ls operation inside a user transaction. Terminate the transaction and reissue the statement.

CHECKTABLE terminated. A failure was detected while collecting facts. Possibly tempdb out of space or a system table is inconsistent. Check previous errors.

Could not repair this error.

The repair level on the DBCC statement caused this repair to be bypassed.

Repairing this error requires other errors to be corrected first.

Table error: Cross object linkage: Page %S_PGID, slot %d, in object ID %d, index ID %d, refers to page %S_PGID, slot %d, in object ID %d, index ID %d.

Table error: Cross object linkage: Parent page %S_PGID, slot %d, in object ID %d, index ID %d, and page %S_PGID, slot %d, in object ID %d, index ID %d, next refer to page %S_PGID but are not in the same object.

Object ID %d, index ID %d: The ghosted record count (%d) in the header does not match the number of ghosted records (%d) found on page %S_PGID.

Object ID %d, index ID %d: Page %S_PGID could not be processed. See other errors for details.

Object ID %d: Errors found in text ID %I64d owned by data record identified by %.*ls.

Table error: Object ID %d, index ID %d cross-object chain linkage. Page %S_PGID points to %S_PGID in object ID %d, index ID %d.

Table error: Object ID %d, index ID %d B-tree level mismatch, page %S_PGID. Level %d does not match level %d from parent %S_PGID.

Table error: Object ID %d, index ID %d, column '%.*ls'. The column ID %d is not valid for this table. The valid range is from 1 to %d.

Table error: Object ID %d, index ID %d. The low key value on page %S_PGID (level %d) is not %ls the key value in the parent %S_PGID slot %d.

Table error: Object ID %d, index ID %d. The high key value on page %S_PGID (level %d) is not less than the low key value in the parent %S_PGID, slot %d of the next page %S_PGID.

Table error: Object ID %d, index ID %d. The previous link %S_PGID on page %S_PGID does not match the previous page %S_PGID that the parent %S_PGID, slot %d expects for this page.

Table error: Object ID %d, index ID %d. B-tree chain linkage mismatch. %S_PGID->next = %S_PGID, but %S_PGID->Prev = %S_PGID.

Table error: Object ID %d, index ID %d. B-tree page %S_PGID has two parent nodes %S_PGID, slot %d and %S_PGID, slot %d.

Table error: Page %S_PGID, Object ID %d, index ID %d. Unexpected page type %d.

Description

Table error: Object ID %d, index ID %d, page %S_PGID. Test (%hs) failed. Values are %ld and %ld.

Table error: Object ID %d, index ID %d, page %S_PGID. Test (%hs) failed. Address 0x%x is not aligned.

Table error: Object ID %d, index ID %d, page %S_PGID. Test (%hs) failed. Slot %d, offset 0x%x is invalid.

Table error: Object ID %d, index ID %d, page %S_PGID. Test (%hs) failed. Slot %d, offset 0x%x overlaps with the prior row.

Table error: Object ID %d, index ID %d, page %S_PGID. Test (%hs) failed. Slot %d, row extends into free space at 0x%x.

Table error: Object ID %d, index ID %d, page %S_PGID, row %d. Test (%hs) failed. Values are %ld and %ld.

Table error: Object ID %d, index ID %d will be rebuilt.

Table error: Allocation page %S_PGID has invalid %ls page header values. Type is %d. Check type, object ID and page ID on the page.

Table error: Multiple IAM pages for object ID %d, index ID %d contain allocations for the same interval. IAM pages %S_PGID and %S_PGID.

Database error: Page %S_PGID is marked with the wrong type in PFS page %S_PGID. PFS status 0x%x expected 0x%x.

%.*ls fixed %d allocation errors and %d consistency errors in table '%ls' (object ID %d).

%.*ls fixed %d allocation errors and %d consistency errors not associated with any single object.

Table error: Table '%ls' (ID %d). Missing or invalid key in index '%ls' (ID %d) for
the row:

Table error: Database '%ls', index '%ls.%ls' (ID %d) (index ID %d). Extra or invalid
key for the keys:

Repair: Deleted text column, text ID %I64d, for object ID %d on page %S_PGID, slot %d.

%.*ls found %d allocation errors and %d consistency errors not associated with any single object.

Data row (%d:%d:%d) identified by (%ls) has index values (%ls).

Index row (%d:%d:%d) with values (%ls) points to the data row identified by (%ls).

DBCC %ls (%ls%ls%ls) executed by %ls found %d errors and repaired %d errors.

%ls is the minimum repair level for the errors found by DBCC %ls (%ls %ls).

Table error: IAM page %S_PGID for object ID %d, index ID %d is linked in the IAM chain for object ID %d, index ID %d by page %S_PGID.

Table error: Page %S_PGID, slot %d, column %d is not a valid complex column.

Description

Table error: Object ID %d. The text, ntext, or image node at page %S_PGID, slot %d, text ID %I64d does not match its reference from page %S_PGID, slot %d.

Table error: The text, ntext, or image node at page %S_PGID, slot %d, text ID %I64d has incorrect node type %d.

Table error: The text, ntext, or image node at page %S_PGID, slot %d, text ID %I64d has type %d. It cannot be placed on a page of type %d.

Table error: Object ID %d. The text, ntext, or image node at page %S_PGID, slot %d, text ID %I64d is not referenced.

Table error: Object ID %d. The text, ntext, or image node at page %S_PGID, slot %d, text ID %I64d is referenced by page %S_PGID, slot %d, but was not seen in the scan.

Could not read and latch page %S_PGID with latch type %ls. %ls failed.

Table error: Invalid value detected in %ls for Object ID %d, index ID %d. Row skipped.

Table error: %ls page %S_PGID (object ID %d, index ID %d) is out of the range of this database.

Table error: IAM chain linkage error: Object ID %d, index ID %d. The next page for IAM page %S_PGID is %S_PGID, but the previous link for page %S_PGID
is %S_PGID.

Row error: Object ID %d, index ID %d, page ID %S_PGID, row ID %d. Column '%.*ls' was created NOT NULL, but is NULL in the row.

Forwarded row mismatch: Object ID %d, page %S_PGID, slot %d points to
forwarded row page %S_PGID, slot %d; the forwarded row points back to page
%S_PGID, slot %d.

Forwarded row referenced by more than one row. Object ID %d, page %S_PGID,
slot %d incorrectly points to forwarded row page %S_PGID, slot %d; the forwarded
row correctly refers back to page %S_PGID, slot %d.

CHECKTABLE processing of object ID %d, index ID %d encountered page %S_PGID, slot %d twice. Possible internal error or allocation fault.

Text node referenced by more than one node. Object ID %d, text, ntext, or image node page %S_PGID, slot %d, text ID %I64d is pointed to by page %S_PGID, slot %d and by page %S_PGID, slot %d.

Table error: Object ID %d, index ID %d. The child page pointer %S_PGID on PageId %S_PGID, slot %d is not a valid page for this database.

Table error: Object ID %d, index ID %d. Page %S_PGID was not seen in the scan although its parent %S_PGID and previous %S_PGID refer to it. Check any previous errors.

Table error: Object ID %d, index ID %d. Parent node for page %S_PGID was not encountered.

Table error: Object ID %d, index ID %d. Page %S_PGID is missing a reference from previous page %S_PGID. Possible chain linkage problem.

Description

Table error: Object ID %d, index ID %d. Page %S_PGID is missing references from parent (unknown) and previous (page %S_PGID) nodes. Possible bad root entry in sysindexes.

Table error: Object ID %d, index ID %d. Index node page %S_PGID, slot %d refers to child page %S_PGID and previous child %S_PGID, but they were not encountered.

Table error: Object ID %d, index ID %d. The next pointer of %S_PGID refers to page %S_PGID. Neither %S_PGID nor its parent were encountered. Possible bad chain linkage.

Table error: Cross object linkage. Page %S_PGID->next in object ID %d, index ID %d refers to page %S_PGID in object ID %d, index ID %d but is not in the same index.

File %d. Extents %d, used pages %d, reserved pages %d, mixed extents %d, mixed pages %d.

Object ID %d, index ID %d. Allocations for %S_PGID. IAM %S_PGID, extents %d, used pages %d, mixed pages %d.

Could not locate file '%.*ls' in sysfiles.

Too many errors found (%d) for object ID %d. To see all error messages rerun the statement using "WITH ALL_ERRORMSGS".

No help available for DBCC statement '%.*ls'.

The schema for database '%ls' is changing. May find spurious allocation problems due to schema changes in progress.

%.*ls found %d allocation errors and %d consistency errors in database '%ls'.

%.*ls found %d allocation errors and %d consistency errors in table '%ls' (object ID %d).

0x%.8x + 0x%.8x bytes is not a valid address range.

Database ID %d, object '%ls' (ID %d). Loop in data chain detected at %S_PGID.

Object ID %d, forwarding row page %S_PGID, slot %d points to page %S_PGID, slot %d. Did not encounter forwarded row. Possible allocation error.

Object ID %d, forwarded row page %S_PGID, slot %d should be pointed to by forwarding row page %S_PGID, slot %d. Did not encounter forwarding row. Possible allocation error.

System table '%.*ls' (object ID %d, index ID %d) is in filegroup %d. All system tables must be in filegroup %d.

IAM page %S_PGID for object ID %d, index ID %d controls pages in filegroup %d, that should be in filegroup %d.

Single page allocation %S_PGID for object ID %d, index ID %d is in filegroup
%d; it should be in filegroup %d.

Page errors on the GAM, SGAM, or PFS pages do not allow CHECKALLOC to verify database ID %d pages from %S_PGID to %S_PGID. See other errors for cause.

Description

Database tempdb allocation errors prevent further %ls processing.

Table 3–64:TSQL Error Codes - 9000 to 9099

Description

The log for database '%.*ls' is not available.

The log file for database '%.*ls' is full. Back up the transaction log for the database to free up some log space.

The LSN %S_LSN passed to log scan in database '%.*ls' is invalid.

An error occurred while processing the log for database '%.*ls'.

Either start LSN or end LSN specified in OpenRowset(DBLog, ...) is invalid.

Cannot shrink log file %d (%s) because total number of logical log files cannot be fewer than %d.

Cannot shrink log file %d (%s) because requested size (%dKB) is larger than the start of the last logical log file.

Cannot shrink log file %d (%s) because all logical log files are in use.

Cannot shrink log file %d (%s) because of minimum log space required.

User does not have permission to query the virtual table, DBLog. Only members of the sysadmin fixed server role and the db_owner fixed database role have this permission

Table 3–65:TSQL Error Codes - 10000 to 10999

Description

10000

10001

10002

10003

10004

10005

10006

10007

10008

10009

10010

10011

10021

Unknown provider error.

The provider reported an unexpected catastrophic failure.

The provider did not implement the functionality.

The provider ran out of memory.

One or more arguments were reported invalid by the provider.

The provider did not support an interface.

The provider indicated an invalid pointer was used.

The provider indicated an invalid handle was used.

The provider terminated the operation.

The provider did not give any information about the error.

The data necessary to complete this operation was not yet available to the provider.

Access denied.

Execution terminated by the provider because a resource limit was reached.

Description

10022

10023

10024

10025

10026

10027

10028

10031

10032

10033

10034

10035

10041

10042

10051

10052

10053

10054

10055

10056

10057

10058

10061

10062

10063

10064

10065

10066

The provider called a method from IRowsetNotify in the consumer, and the method has not yet returned.

The provider does not support the necessary method.

The provider indicates that the user did not have the permission to perform the operation.

Provider caused a server fault in an external process.

No command text was set.

Command was not prepared.

Authentication failed.

An error occurred because one or more properties could not be set.

Cannot return multiple result sets (not supported by the provider).

The specified index does not exist or the provider does not support an index scan on this data source.

The specified table does not exist.

No value was given for one or more of the required parameters.

Could not set any property values.

Cannot set any properties while there is an open rowset.

An error occurred while setting the data.

The insertion was canceled by the provider during notification.

Could not convert the data value due to reasons other than sign mismatch or overflow.

The data value for one or more columns overflowed the type used by the provider.

The data violated the integrity constraints for one or more columns.

The number of rows that have pending changes has exceeded the limit specified by the DBPROP_MAXPENDINGROWS property.

Cannot create the row. Would exceed the total number of active rows supported by the rowset.

The consumer cannot insert a new row before releasing previously-retrieved row handles.

An error occurred while setting data for one or more columns.

The change was canceled by the provider during notification.

Could not convert the data value due to reasons other than sign mismatch or overflow.

The data value for one or more columns overflowed the type used by the provider.

The data violated the integrity constraints for one or more columns.

The number of rows that have pending changes has exceeded the limit specified by the DBPROP_MAXPENDINGROWS property.

Description

10067

10068

10069

10075

10081

10085

10086

10087

10088

The rowset was using optimistic concurrency and the value of a column has been changed after the containing row was last fetched or resynchronized.

The consumer could not delete the row. A deletion is pending or has already been transmitted to the data source.

The consumer could not delete the row. The insertion has been transmitted to the data source.

An error occurred while deleting the row.

The rowset uses integrated indexes and there is no current index.

RestartPosition on the table was canceled during notification.

The table was built over a live data stream and the position cannot be restarted.

The provider did not release some of the existing rows.

The order of the columns was not specified in the object that created the rowset. The provider had to reexecute the command to reposition the next fetch position to its initial position, and the order of the columns changed.

Table 3–66:TSQL Error Codes - 11000 to 11999

Description

11000

11001

11002

11003

11004

11005

11006

11007

11008

11009

11010

11011

11012

11013

11014

11015

11031

Unknown status code for this column.

Non-NULL value successfully returned.

Deferred accessor validation occurred. Invalid binding for this column.

Could not convert the data value due to reasons other than sign mismatch or overflow.

Successfully returned a NULL value.

Successfully returned a truncated value.

Could not convert the data type because of a sign mismatch.

Conversion failed because the data value overflowed the data type used by the provider.

The provider cannot allocate memory or open another storage object on this column.

The provider cannot determine the value for this column.

The user did not have permission to write to the column.

The data value violated the integrity constraints for the column.

The data value violated the schema for the column.

The column had a bad status.

The column used the default value.

The column was skipped when setting data.

The row was successfully deleted.

Description

11032

11033

11034

11036

11037

11038

11039

11040

11041

11042

11043

11044

11045

11046

11047

11048

11100

11101

11102

11103

11104

11105

11106

11107

11108

11109

The table was in immediate-update mode, and deleting a single row caused more than one row to be deleted in the data source.

The row was released even though it had a pending change.

Deletion of the row was canceled during notification.

The rowset was using optimistic concurrency and the value of a column has been changed after the containing row was last fetched or resynchronized.

The row has a pending delete or the deletion had been transmitted to the data source.

The row is a pending insert row.

DBPROP_CHANGEINSERTEDROWS was VARIANT_FALSE and the insertion for the row has been transmitted to the data source.

Deleting the row violated the integrity constraints for the column or table.

The row handle was invalid or was a row handle to which the current thread does not have access rights.

Deleting the row would exceed the limit for pending changes specified by the rowset property DBPROP_MAXPENDINGROWS.

The row has a storage object open.

The provider ran out of memory and could not fetch the row.

User did not have sufficient permission to delete the row.

The table was in immediate-update mode and the row was not deleted due to reaching a limit on the server, such as query execution timing out.

Updating did not meet the schema requirements.

There was a recoverable, provider-specific error, such as an RPC failure.

The provider indicates that conflicts occurred with other properties or requirements.

Could not obtain an interface required for text, ntext, or image access.

The provider could not support a required row lookup interface.

The provider could not support an interface required for the
UPDATE/DELETE/INSERT statements.

The provider could not support insertion on this table.

The provider could not support updates on this table.

The provider could not support deletion on this table.

The provider could not support a row lookup position.

The provider could not support a required property.

The provider does not support an index scan on this data source.

Table 3–67:TSQL Error Codes - 13000 to 13999

Description

13001

13002

13003

13004

13005

13006

13007

13008

13009

13010

13011

13012

13013

13014

13015

13016

13017

13018

13019

13020

13021

13022

13023

13024

13025

13026

13027

13028

13029

13030

13031

13032

13033

data page

index page

leaf page

last

root

read from

send to

receive

send

read

wait

a USE database statement

a procedure or trigger

a DISTINCT clause

a view

an INTO clause

an ORDER BY clause

a COMPUTE clause

a SELECT INTO statement

option

offset option

statistics option

parameter option

function name

varbinary (128) NOT NULL

parameter

convert specification

index

table

database

procedure

trigger

view

Description

13034

13035

13036

13037

13038

13039

13040

13041

13042

13043

13044

13045

13046

13047

13048

13076

13077

13078

13079

13080

13081

13082

13083

13084

13085

13086

13087

13088

default

rule

system table

unknown type

SET statement

column

type

character string

integer

identifier

number

integer value

floating point value

object

column heading

an assignment

a cursor declaration

replication filter

variable assignment

statistics

file

filegroup

server

write

function

database collation

drop

alter

Table 3–68:TSQL Error Codes - 14000 to 14999

Description

14002

14003

14004

Could not find the 'Sync' subsystem with the task ID %ld.

You must supply a publication name.

%s must be in the current database.

Description

14005

14006

14008

14009

14010

14012

14013

14014

14015

14016

14017

14018

14019

14020

14021

14022

14023

14025

14027

14028

14029

14030

14031

14032

14033

14034

14035

14036

Could not drop publication. A subscription exists to it.

Could not drop the publication.

There are no publications.

There are no articles for publication '%s'.

The remote server is not defined as a subscription server.

The @status parameter value must be either 'active' or 'inactive'.

This database is not enabled for publication.

The synchronization method (@sync_method) must be '[bcp] native', '[bcp] character', 'concurrent' or 'concurrent_c'.

The replication frequency (@repl_freq) must be either 'continuous' or 'snapshot'.

The publication '%s' already exists.

Invalid @restricted parameter value. Valid options are 'true' or 'false'.

Could not create the publication.

The @operation parameter value must be either 'add' or 'drop'.

Could not obtain the column ID for the specified column. Schema replication failed.

The column was not added correctly to the article.

The @property parameter value must be either 'description', 'sync_object', 'type', 'ins_cmd', 'del_cmd', 'upd_cmd', 'filter', 'dest_table', 'dest_object', 'creation_script', 'pre_creation_cmd', 'status', 'schema_option', or 'destination_owner'.

The type must be '[indexed view] logbased', '[indexed view] logbased manualfilter', '[indexed view] logbased manualview', '[indexed view] logbased manualboth', or '( view | indexed view | proc | func ) schema only'.

Article update successful.

%s does not exist in the current database.

Only user tables, materialized views, and stored procedures can be published as 'logbased' articles.

The vertical partition switch must be either 'true' or 'false'.

The article '%s' exists in publication '%s'.

User tables and views are the only valid synchronization objects.

The value of parameter %s cannot be 'all'. It is reserved by replication stored procedures.

Could not change replication frequency because there are active subscriptions on the publication.

The publication name (@publication) cannot be the keyword 'all'.

The replication option '%s' of database '%s' has already been set to true.

Could not enable database for publishing.

Description

14037

14038

14039

14040

14042

14043

14046

14047

14048

14049

14050

14051

14052

14053

14054

14055

14056

14057

14058

14059

14061

14062

14063

14065

14066

14067

14068

14069

14070

14071

The replication option '%s' of database '%s' has been set to false.

Could not disable database for publishing.

Could not construct column clause for article view. Reduce the number of columns or create the view manually.

The server '%s' is already a Subscriber.

Could not create Subscriber.

The parameter %s cannot be NULL.

Could not drop article. A subscription exists on it.

Could not drop %s.

The server '%s' is not a Subscriber.

Stored procedures for replication are the only objects that can be used as a filter.

No subscription is on this publication or article.

The parameter value must be 'sync_type' or 'dest_db'.

The @sync_type parameter value must be 'automatic' or 'none'.

The subscription could not be updated at this time.

The subscription was updated successfully.

The subscription does not exist.

The subscription could not be dropped at this time.

The subscription could not be created.

The subscription already exists.

Materialized view articles cannot be created for publications with the properties allow_sync_tran, allow_queued_tran, or allow_dts.

The @pre_creation_cmd parameter value must be 'none', 'drop', 'delete', or 'truncate'.

The Subscriber was dropped.

The remote server does not exist or has not been designated as a valid Subscriber.

The @status parameter value must be 'initiated', 'active', 'inactive', or 'subscribed'.

The previous status must be 'active', 'inactive', or 'subscribed'.

The status value is the same as the previous status value.

Could not update sysobjects. The subscription status could not be changed.

Could not update sysarticles. The subscription status could not be changed.

Could not update the distribution database subscription table. The subscription status could not be changed.

Could not find the Distributor or the distribution database for the local server. The Distributor may not be installed, or the local server may not be configured as a Publisher at the Distributor.

Description

14074

14075

14076

14077

14078

14080

14085

14088

14089

14090

14091

14092

14093

14094

14095

14096

14097

14098

14099

The server '%s' is already listed as a Publisher.

The Publisher could not be created at this time.

Could not grant replication login permission to '%s'.

The publication was updated successfully.

The parameter must be 'description', 'taskid', 'sync_method', 'status', 'repl_freq', 'restricted', 'retention', 'immediate_sync', 'enabled_for_internet', 'allow_push', 'allow_pull', 'allow_anonymous', or 'retention'.

The remote server does not exist or has not been designated as a valid Publisher.

The Subscriber information could not be obtained from the Distributor.

The table '%s' must have a primary key to be published using the transaction-based method.

The clustered index on materialized view '%s' may not contain nullable columns if it is to be published using the transaction-based method.

Error evaluating article synchronization object after column drop. The filter clause for article '%s' must not reference the dropped column.

The @type parameter passed to sp_helpreplicationdb must be either 'pub' or 'sub'.

Could not change article because there is an existing subscription to the article.

Cannot grant or revoke access directly on publication '%s' because it uses the default publication access list.

Could not subscribe to article '%s' because heterogeneous Subscriber '%s' does not support the @pre_creation_cmd parameter value 'truncate'.

Could not subscribe to publication '%s' because heterogeneous Subscriber '%s' only supports the @sync_method parameter value 'bcp character' .

The path and name of the table creation script must be specified if the @pre_creation_cmd parameter value is 'drop'.

The 'status' value must be 'no column names', 'include column names', 'string literals', 'parameters', 'DTS horizontal partitions' or 'no DTS horizontal partitions'.

Cannot drop Distribution Publisher '%s'. The remote Publisher is using '%s' as
Distributor.

The server '%s' is already defined as a Distributor.

Table 3–69:TSQL Error Codes - 14100 to 14199

Description

14100

14101

14102

Specify all articles when subscribing to a publication using concurrent snapshot processing.

The publication '%s' already has a Snapshot Agent defined.

Specify all articles when unsubscribing from a publication using concurrent snapshot processing.

Description

14105

14106

14107

14108

14109

14110

14111

14112

14113

14114

14115

14117

14118

14119

14120

14121

14122

14123

14124

14126

14128

14129

14135

14136

14137

14138

14139

You have updated the distribution database property '%s' successfully.

Distribution retention periods must be greater than 0.

The @max_distretention value must be larger than the @min_distretention value.

Removed %ld history records from %s.

The @security_mode parameter value must be 0 (SQL Server Authentication) or
## 1 (Windows Authentication).

For stored procedure articles, the @property parameter value must be 'description', 'dest_table', 'dest_object', 'creation_script', 'pre_creation_cmd', 'schema_option', or 'destination_owner'.

The @pre_creation_cmd parameter value must be 'none' or 'drop'.

This procedure can be executed only against table-based articles.

Could not execute '%s'. Check '%s' in the install directory.

'%s' is not configured as a Distributor.

The property parameter value must be %s.

'%s' is not configured as a distribution database.

A stored procedure can be published only as a 'serializable proc exec' article, a 'proc exec' article, or a 'proc schema only' article.

Could not add the distribution database '%s'. This distribution database already exists.

Could not drop the distribution database '%s'. This distributor database is associated with a Publisher.

Could not drop the Distributor '%s'. This Distributor has associated distribution databases.

The @article parameter value must be 'all' for immediate_sync publications.

The subscription @sync_type parameter value 'manual' is no longer supported.

A publication must have at least one article before a subscription to it can be created.

You do not have the required permissions to complete the operation.

Invalid @subscription_type parameter value. Valid options are 'push' or 'pull'.

The @status parameter value must be NULL for 'automatic' sync_type when you add subscriptions to an immediate_sync publication.

There is no subscription on Publisher '%s', publisher database '%s', publication '%s'.

The keyword 'all' is reserved by replication stored procedures.

The @value parameter value must be either 'true' or 'false'.

Invalid option name '%s'.

The replication system table '%s' already exists.

Description

14143

14144

14146

14147

14148

14149

14150

14151

14152

14153

14154

14155

14156

14157

14158

14159

14199

Cannot drop Distributor Publisher '%s'. There are Subscribers associated with it in the distribution database '%s'.

Cannot drop Subscriber '%s'. There are subscriptions from it in the publication database '%s'.

The article parameter '@schema_option' cannot be NULL.

Restricted publications are no longer supported.

Invalid '%s' value. Valid values are 'true' or 'false'.

Removed %ld replication history records in %s seconds (%ld row/secs).

Replication-%s: agent %s succeeded. %s

Replication-%s: agent %s failed. %s

Replication-%s: agent %s scheduled for retry. %s

Replication-%s: agent %s warning. %s

The Distributor parameter must be '@heartbeat_interval'.

Invalid article ID specified for procedure script generation.

The custom stored procedure was not specified in the article definition.

The subscription created by Subscriber '%s' to publication '%s' has expired and has been dropped.

Replication-%s: agent %s: %s.

Could not change property '%s' for article '%s' because there is an existing subscription to the article.

The specified job "%s" is not created for maintenance plans.

Table 3–70:TSQL Error Codes - 14200 to 14299

Description

14200

14201

14202

14203

14204

14205

14206

14207

14208

14209

14210

The specified '%s' is invalid.

## 0 (all steps) ..

before or after @active_start_time

sp_helplogins [excluding Windows NT groups]

(suspended), 7 (performing completion actions)

(unknown)

0..n seconds

-1 [no maximum], 0..n

1..7 [1 = E-mail, 2 = Pager, 4 = NetSend]

0..127 [1 = Sunday .. 64 = Saturday]

notification

Description

14211

14212

14213

14214

14215

14216

14217

14218

14219

14220

14221

14222

14223

14224

14225

14226

14227

14228

14229

14230

14231

14232

14233

14234

14235

14236

14237

14238

14239

14240

server

(all jobs)

Core Job Details:

Job Steps:

Job Schedules:

Job Target Servers:

SQL Server Warning: '%s' has performed a forced defection of TSX server '%s'. Run sp_delete_targetserver at the MSX in order to complete the defection.

hour

minute

second

This job has one or more notifications to operators other than '%s'. The job cannot be targeted at remote servers as currently defined.

Cannot rename the '%s' operator.

Cannot modify or delete operator '%s' while this server is a %s.

Warning: The server name given is not the current MSX server ('%s').

Warning: Could not determine local machine name. This prevents MSX operations from being posted.

%ld history entries purged.

Server defected from MSX '%s'. %ld job(s) deleted.

Server MSX enlistment changed from '%s' to '%s'.

Server enlisted into MSX '%s'.

SP_POST_MSX_OPERATION: %ld %s download instruction(s) posted.

SP_POST_MSX_OPERATION Warning: The specified %s ('%s') is not involved in a multiserver job.

Specify either a job_name, job_id, or an originating_server.

Specify a valid job_id (or 0x00 for all jobs).

The specified '%s' is invalid (valid values are returned by %s).

The specified '%s' is invalid (valid values are greater than 0 but excluding %ld).

Warning: Non-existent step referenced by %s.

When an action of 'REASSIGN' is specified, the New Login parameter must also be supplied.

%ld jobs deleted.

%ld jobs reassigned to %s.

Job applied to %ld new servers.

Description

14241

14242

14243

14244

14245

14246

14247

14248

14249

14250

14251

14252

14253

14254

14255

14256

14257

14258

14259

14260

14261

14262

14263

14264

14265

14266

14267

14268

14269

14270

14271

14272

Job removed from %ld servers.

Only a system administrator can reassign ownership of a job.

Job '%s' started successfully.

Only a system administrator can reassign tasks.

Specify either the @name, @id, or @loginname of the task(s) to be deleted.

Specify either the @currentname or @id of the task to be updated.

Only a system administrator can view tasks owned by others.

This login is the owner of %ld job(s). You must delete or reassign these jobs before the login can be dropped.

Specify either @taskname or @oldloginname when reassigning a task.

The specified %s is too long. It must contain no more than %ld characters.

Cannot specify '%s' as the operator to be notified.

Cannot perform this action on a job you do not own.

%ld (of %ld) job(s) stopped successfully.

Job '%s' stopped successfully.

The owner ('%s') of this job is either an invalid login, or is not a valid user of database '%s'.

Cannot start job '%s' (ID %s) because it does not have any job server(s) defined.

Cannot stop job '%s' (ID %s) because it does not have any job server(s) defined.

Cannot perform this operation while SQLServerAgent is starting. Try again later.

A schedule (ID %ld, '%s') for this job with this definition already exists.

You do not have sufficient permission to run this command.

The specified %s ('%s') already exists.

The specified %s ('%s') does not exist.

Target server '%s' is already a member of group '%s'.

Target server '%s' is not a member of group '%s'.

The MSSQLServer service terminated unexpectedly.

The specified '%s' is invalid (valid values are: %s).

Cannot add a job to the '%s' job category.

There are no jobs at this server that originated from server '%s'.

Job '%s' is already targeted at server '%s'.

Job '%s' is not currently targeted at server '%s'.

A target server cannot be named '%s'.

Object-type and object-name must be supplied as a pair.

Description

14273

14274

14275

14276

14277

14278

14279

14280

14281

14282

14283

14284

14285

14286

14287

14288

14289

14290

14291

14292

14293

14294

14295

14296

14297

14298

14299

You must provide either @job_id or @job_name (and, optionally, @schedule_name), or @schedule_id.

Cannot add, update, or delete a job (or its steps or schedules) that originated from an MSX server.

The originating server must be either '(local)' or '%s'.

'%s' is a permanent %s category and cannot be deleted.

The command script does not destroy all the objects that it creates. Revise the command script.

The schedule for this job is invalid (reason: %s).

Supply either @job_name or @originating_server.

Supply either a job name (and job aspect), or one or more job filter parameters.

Warning: The @new_owner_login_name parameter is not necessary when specifying a 'DELETE' action.

Supply either a date (created or last modified) and a data comparator, or no date parameters at all.

Supply @target_server_groups or @target_servers, or both.

Cannot specify a job ID for a new job. An ID will be assigned by the procedure.

Cannot add a local job to a multiserver job category.

Cannot add a multiserver job to a local job category.

The '%s' supplied has an invalid %s.

%s cannot be before %s.

%s cannot contain '%s' characters.

This job is currently targeted at the local server so cannot also be targeted at a remote server.

This job is currently targeted at a remote server so cannot also be targeted at the local server.

There are two or more tasks named '%s'. Specify %s instead of %s to uniquely identify the task.

There are two or more jobs named '%s'. Specify %s instead of %s to uniquely identify the job.

Supply either %s or %s to identify the job.

Frequency Type 0x2 (OnDemand) is no longer supported.

This server is already enlisted into MSX '%s'.

Cannot enlist into the local machine.

This server is not currently enlisted into an MSX.

Server '%s' is an MSX. Cannot enlist one MSX into another MSX.

Table 3–71:TSQL Error Codes - 14300 to 14399

Description

14300

14301

14302

14303

14304

14350

14351

14352

14353

14354

14355

14356

14357

14358

Circular dependencies exist. Dependency evaluation cannot continue.

Logins other than the current user can only be seen by members of the sysadmin role.

You must upgrade your client to version 6.5 of SQL-DMO and SQL Server Enterprise Manager to connect to this server. The upgraded versions will administer both SQL Server version 6.5 and 6.0 (if sqlole65.sql is run).

Stored procedure '%s' failed to access registry key.

Stored procedure '%s' can run only on Windows 2000 servers.

Cannot initialize COM library because CoInitialize failed.

Cannot complete this operation because an unexpected error occurred.

Cannot find Active Directory information in the registry for this SQL Server instance. Run sp_ActiveDirectory_SCP again.

Cannot determine the service account for this SQL Server instance.

Cannot start the MSSQLServerADHelper service. Verify that the service account for this SQL Server instance has the necessary permissions to start the MSSQLServerADHelper service.

The MSSQLServerADHelper service is busy. Retry this operation later.

The Windows Active Directory client is not installed properly on the computer where this SQL Server instance is running. LoadLibrary failed to load
ACTIVEDS.DLL.

Cannot list '%s' in Active Directory because the name is too long. Active Directory common names cannot exceed 64 characters.

Cannot determine the SQL Server Agent proxy account for this SQL Server instance or the account is not a domain user account. Use xp_sqlagent_proxy_account to configure SQL Server Agent to use a domain user account as the proxy account.

Table 3–72:TSQL Error Codes - 14400 to 14499

Description

14410

14411

14412

14413

14414

14415

14416

14417

You must supply either a plan_name or a plan_id.

Cannot delete this plan. The plan contains enlisted databases.

The destination database is already part of a log shipping plan.

This database is already log shipping.

A log shipping monitor is already defined.

The user name cannot be null when using SQL Server authentication.

This stored procedure must be run in msdb.

Cannot delete the monitor server while databases are participating in log shipping.

Description

14418

14419

14420

14421

14422

14423

14424

14425

14426

14427

14428

14429

14430

14440

14441

14442

14450

14451

The specified @backup_file_name was not created from database '%s'.

The specified @backup_file_name is not a database backup.

The log shipping source %s.%s has not backed up for %s minutes.

The log shipping destination %s.%s is out of sync by %s minutes.

Supply either @plan_id or @plan_name.

Other databases are enlisted on this plan and must be removed before the plan can be deleted.

The database '%s' is already involved in log shipping.

The database '%s' does not seem to be involved in log shipping.

A log shipping monitor is already defined. Call sp_define_log_shipping_monitor with @delete_existing = 1.

A user name is necessary for SQL Server security.

Could not remove the monitor as there are still databases involved in log shipping.

There are still secondary servers attached to this primary.

Invalid destination path %s.

Could not set single user mode.

Role change succeeded.

Role change failed.

The specified @backup_file_name was not taken from database '%s'.

The specified @backup_file_name is not a database backup.

Table 3–73:TSQL Error Codes - 14500 to 14599

Description

14500

14501

14502

14503

14504

14505

14506

14507

Supply either a non-zero message ID, non-zero severity, or non-null performance condition.

An alert ('%s') has already been defined on this condition.

The @target_name parameter must be supplied when specifying an @enum_type of 'TARGET'.

The @target_name parameter should not be supplied when specifying an @enum_type of 'ALL' or 'ACTUAL'.

'%s' is the fail-safe operator.You must make another operator the fail-safe operator before '%s' can be dropped.

Specify a null %s when supplying a performance condition.

Cannot set alerts on message ID %ld.

A performance condition must be formatted as:
'object_name|counter_name|instance_name|comparator(> or < or =)|numeric value'.

Description

14539

14540

14541

14542

14543

14544

14545

14546

14547

14548

14549

14550

14551

14552

14553

14554

14555

14556

14557

14558

14559

14560

14561

14562

14563

14564

14565

14566

14567

14568

Only a Standard or Enterprise edition of SQL Server can be enlisted into an MSX.

Only a SQL Server running on Microsoft Windows NT can be enlisted into an
MSX.

The version of the MSX (%s) is not recent enough to support this TSX. Version %s or later is required at the MSX.

It is invalid for any TSQL step of a multiserver job to have a non-null %s value.

Login '%s' owns one or more multiserver jobs. Ownership of these jobs can only be assigned to members of the %s role.

This job is owned by '%s'. Only a job owned by a member of the %s role can be a multiserver job.

The %s parameter is not valid for a job step of type '%s'.

The %s parameter is not supported on Windows 95/98 platforms.

Warning: This change will not be downloaded by the target server(s) until an %s for the job is posted using %s.

Target server '%s' does not have any jobs assigned to it.

(Description not requested.)

Command-Line Subsystem

Replication Snapshot Subsystem

Replication Transaction-Log Reader Subsystem

Replication Distribution Subsystem

Replication Merge Subsystem

Active Scripting Subsystem

Transact-SQL Subsystem

[Internal]

(encrypted command)

(append output file)

(include results in history)

(normal)

(quit with success)

(quit with failure)

(goto next step)

(goto step)

(idle)

(below normal)

(above normal)

14569

14570

14571

14572

14573

14574

14575

14576

14577

14578

14579

14580

14581

14585

14586

14587

14588

14589

14590

14591

14592

14593

14594

14595

14596

14597

14598

14599

Description

(time critical)

(Job outcome)

No description available.

@freq_interval must be at least 1 for a daily job.

@freq_interval must be a valid day of the week bitmask [Sunday = 1 .. Saturday = 64] for a weekly job.

@freq_interval must be between 1 and 31 for a monthly job.

@freq_relative_interval must be one of 1st (0x1), 2nd (0x2), 3rd [0x4], 4th (0x8) or Last (0x10).

@freq_interval must be between 1 and 10 (1 = Sunday .. 7 = Saturday, 8 = Day,
## 9 = Weekday, 10 = Weekend-day) for a monthly-relative job.

@freq_recurrence_factor must be at least 1.

Starts whenever the CPU usage has remained below %ld percent for %ld seconds.

Automatically starts when SQLServerAgent starts.

job

Replication Transaction Queue Reader Subsystem

Only the owner of DTS Package '%s' or a member of the sysadmin role may reassign its ownership.

Only the owner of DTS Package '%s' or a member of the sysadmin role may create new versions of it.

Only the owner of DTS Package '%s' or a member of the sysadmin role may drop it or any of its versions.

ID.VersionID =

[not specified]

DTS Package '%s' already exists with a different ID in this category.

DTS Category '%s' already exists in the specified parent category.

DTS Category '%s' was found in multiple parent categories. You must uniquely specify the category to be dropped.

DTS Category '%s' contains packages and/or other categories. You must drop these first, or specify a recursive drop.

DTS Package

DTS Package '%s' exists in different categories. You must uniquely specify the package.

DTS Package '%s' exists in another category.

DTS Package ID '%s' already exists with a different name.

Cannot drop the Local, Repository, or LocalDefault DTS categories.

Name

Table 3–74:TSQL Error Codes - 15000 to 15099

Description

15001

15002

15003

15004

15005

15006

15007

15008

15009

15010

15011

15012

15013

15014

15015

15016

15017

15018

15019

15020

15021

15022

15023

15024

15025

15026

15027

15028

15029

15030

15031

15032

15033

Object '%ls' does not exist or is not a valid object for this operation.

The procedure '%s' cannot be executed within a transaction.

Only members of the %s role can execute this stored procedure.

Name cannot be NULL.

Statistics for all tables have been updated.

'%s' is not a valid name because it contains invalid characters.

The login '%s' does not exist.

User '%s' does not exist in the current database.

The object '%s' does not exist in database '%s'.

The database '%s' does not exist. Use sp_helpdb to show available databases.

Database option '%s' does not exist.

The device '%s' does not exist. Use sp_helpdevice to show available devices.

Table '%s': No columns without statistics found.

The role '%s' does not exist in the current database.

The server '%s' does not exist. Use sp_helpserver to show available servers.

The default '%s' does not exist.

The rule '%s' does not exist.

Table '%s': Creating statistics for the following columns:

The extended stored procedure '%s' does not exist.

Statistics have been created for the %d listed columns of the above tables.

There are no remote users mapped to any local user from remote server '%s'.

The specified user name is already aliased.

User or role '%s' already exists in the current database.

The group '%s' already exists in the current database.

The login '%s' already exists.

Logical device '%s' already exists.

There are no remote users mapped to local user '%s' from remote server '%s'.

The server '%s' already exists.

The data type '%s' already exists in the current database.

The read-only bit cannot be turned off because the database is in standby mode.

'Virtual_device' device added.

The database '%s' already exists.

'%s' is not a valid official language name.

Description

15034

15035

15036

15037

15038

15039

15040

15041

15043

15044

15045

15046

15047

15048

15049

15050

15051

15052

15053

15054

15055

15056

15057

15058

15059

15060

15061

15062

15063

15064

15065

15066

The application role password must not be NULL.

'%s' is not a database device.

The data type '%s' does not exist.

The physical data type '%s' does not allow nulls.

User-defined data types based on timestamp are not allowed.

The language %s already exists in syslanguages.

User-defined error messages must have an ID greater than 50000.

User-defined error messages must have a severity level between 1 and 25.

You must specify 'REPLACE' to overwrite an existing message.

'%s' is an unknown device type. Use 'disk', 'tape', or 'pipe'.

The logical name cannot be NULL.

The physical name cannot be NULL.

The only permitted options for a tape device are 'skip' and 'noskip'.

Valid values of database compatibility level are %d, %d, %d, or %d.

Cannot unbind from '%s'. Use ALTER TABLE DROP CONSTRAINT.

Cannot bind default '%s'. The default must be created using the CREATE DEFAULT statement.

Cannot rename the table because it is published for replication.

Prior to updating sysdatabases entry for database '%s', mode = %d and status = %d (status suspect_bit = %d).

Objects exist which are not owned by the database owner.

The current compatibility level is %d.

Error. Updating sysdatabases returned @@error <> 0.

No row in sysdatabases was updated because mode and status are already correctly reset. No error and no changes made.

List of %s name contains spaces, which are not allowed.

List of %s has too few names.

List of %s has too many names.

List of %s names contains name(s) which have '%s' non-alphabetic characters.

Add device request denied. A physical device named '%s' already exists.

The guest user cannot be mapped to a login name.

The login already has an account under a different user name.

PRIMARY KEY and UNIQUE KEY constraints do not have space allocated.

All user IDs have been assigned.

A default-name mapping of a remote login from remote server '%s' already exists.

Description

15067

15068

15069

15070

15071

15072

15073

15074

15075

15076

15077

15078

15079

15081

15082

15083

15084

15085

15086

15087

15088

15089

15090

15091

15092

15093

15094

15095

15097

'%s' is not a local user. Remote login denied.

A remote user '%s' already exists for remote server '%s'.

One or more users are using the database. The requested operation cannot be completed.

Object '%s' was successfully marked for recompilation.

Usage: sp_addmessage <msgnum>,<severity>,<msgtext> [,<language> [,FALSE | TRUE [,REPLACE]]]

Usage: sp_addremotelogin remoteserver [, loginame [,remotename]]

For row in sysdatabases for database '%s', the status bit %d was forced off and mode was forced to 0.

Warning: You must recover this database prior to access.

The data type '%s' is reserved for future use.

Default, table, and user data types must be in the current database.

Rule, table, and user data type must be in the current database.

The table or view must be in the current database.

Queries processed: %d.

Membership of the public role cannot be changed.

NULL is not an acceptable parameter value for this procedure. Use a percent sign instead.

Physical data type '%s' does not accept a collation

The column or user data type must be in the current database.

Usage: sp_addtype name, 'data type' [,'NULL' | 'NOT NULL']

Invalid precision specified. Precision must be between 1 and 38.

Invalid scale specified. Scale must be less than precision and positive.

The physical data type is fixed length. You cannot specify the length.

Cannot change the '%s' option of a database while another user is in the database.

There is already a local server.

You must specify a length with this physical data type.

Invalid length specified. Length must be between 1 and 8000 bytes.

'%s' is not a valid date order.

'%s' is not a valid first day.

Insert into syslanguages failed. Language not added.

The size associated with an extended property cannot be more than 7,500 bytes.

Table 3–75:TSQL Error Codes - 15100 to 15199

Description

15100

15101

15102

15103

15104

15105

15106

15107

15109

15110

15111

15112

15123

15124

15125

15126

15127

15129

15130

15131

15132

15133

15134

15135

15139

15140

15142

15143

15144

15174

Usage: sp_bindefault defaultname, objectname [, 'futureonly']

Cannot bind a default to a column of data type timestamp.

Cannot bind a default to an identity column.

Cannot bind a default to a column created with or altered to have a default value.

You do not own a table named '%s' that has a column named '%s'.

You do not own a data type with that name.

Usage: sp_bindrule rulename, objectname [, 'futureonly']

Cannot bind a rule to a column of data type text, ntext, image, or timestamp.

Cannot change the owner of the master database.

The proposed new database owner is already a user in the database.

The proposed new database owner is already aliased in the database.

The third parameter for table option 'text in row' is invalid. It should be 'on', 'off', '0', or a number from 24 through 7000.

The configuration option '%s' does not exist, or it may be an advanced option.

The configuration option '%s' is not unique.

Trigger '%s' is not a trigger for '%s'.

Trigger '%s' was not found.

Cannot set the default language to a language ID not defined in syslanguages.

'%d' is not a valid value for configuration option '%s'.

Table '%s' already has a '%s' trigger for '%s'.

Usage: sp_dbremove <dbname> [,dropdev]

Cannot change default database belonging to someone else.

INSTEAD OF trigger '%s' cannot be associated with an order.

No alias exists for the specified user.

Object is invalid. Extended properties are not permitted on '%s', or the object does not exist.

The device is a RAM disk and cannot be used as a default device.

Usage: sp_diskdefault logicalname {defaulton | defaultoff}

Cannot drop the role '%s'.

'%s' is not a valid option for the @updateusage parameter. Enter either 'true' or 'false'.

The role has members. It must be empty before it can be dropped.

Login '%s' owns one or more database(s). Change the owner of the following
database(s) before dropping login:

Description

15175

15176

15177

15178

15179

15180

15181

15182

15183

15184

15185

15190

15191

15193

15194

15197

15198

Login '%s' is aliased or mapped to a user in one or more database(s). Drop the user or alias before dropping the login.

The only valid @parameter value is 'WITH_LOG'.

Usage: sp_dropmessage <msg number> [,<language> | 'ALL']

Cannot drop a message with an ID less than 50000.

Message number %u does not exist.

Cannot drop. The data type is being used.

Cannot drop the database owner.

Cannot drop the guest user from master or tempdb.

The user owns objects in the database and cannot be dropped.

The user owns data types in the database and cannot be dropped.

There is no remote user '%s' mapped to local user '%s' from the remote server '%s'.

There are still remote logins for the server '%s'.

Usage: sp_dropserver server [, droplogins]

This procedure can only be used on system tables.

Cannot re-create index on this table.

There is no text for object '%s'.

The name supplied (%s) is not a user, role, or aliased login.

Table 3–76:TSQL Error Codes - 15200 to 15299

Description

15200

15201

15202

15203

15204

15205

15206

15210

15211

15216

15217

15218

There are no remote servers defined.

There are no remote logins for the remote server '%s'.

There are no remote logins defined.

There are no remote logins for '%s'.

There are no remote logins for '%s' on remote server '%s'.

There are no servers defined.

Invalid Remote Server Option: '%s'.

Only members of the sysadmin role can use the loginame option. The password was not changed.

Old (current) password incorrect for user. The password was not changed.

'%s' is not a valid option for the @delfile parameter.

Property cannot be updated or deleted. Property '%s' does not exist for '%s'.

Object '%s' is not a table.

Description

15220

15221

15222

15223

15224

15225

15227

15228

15233

15234

15235

15236

15237

15238

15239

15241

15242

15243

15244

15245

15247

15248

15249

15250

15251

15252

15253

15254

Usage: sp_remoteoption [remoteserver, loginame, remotename, optname, {true
| false}]

Remote login option does not exist or cannot be set by user. Run sp_remoteoption with no parameters to see options.

Remote login option '%s' is not unique.

Error: The input parameter '%s' is not allowed to be null.

Error: The value for the @newname parameter contains invalid characters or violates a basic restriction (%s).

No item by the name of '%s' could be found in the current database '%s', given that @itemtype was input as '%s'.

The database '%s' cannot be renamed.

A member of the sysadmin role must set database '%s' to single user mode with sp_dboption before it can be renamed.

Property cannot be added. Property '%s' already exists for '%s'.

Object is stored in sysprocedures and has no space allocated directly.

Views do not have space allocated.

Column '%s' has no default.

User data type '%s' has no default.

Column '%s' has no rule.

User data type '%s' has no rule.

Usage: sp_dboption [dbname [,optname [,'true' | 'false']]]

Database option '%s' is not unique.

The option '%s' cannot be changed for the master database.

Only members of the sysadmin role or the database owner may set database options.

DBCC DBCONTROL error. Database was not placed offline.

User does not have permission to perform this action.

Error: The parameter @oldname is either ambiguous or the claimed @itemtype (%s) was wrong.

Error: Explicit @itemtype '%s' is unrecognized (%d).

The database name component of the object qualifier must be the name of the current database.

Invalid '%s' specified. It must be %s.

The primary or foreign key table name must be given.

Syntax error parsing SQL identifier '%s'.

Users other than the database owner or guest exist in the database. Drop them before removing the database.

Description

15255

15256

15257

15258

15261

15262

15264

15266

15269

15270

15271

15275

15277

15278

15279

15280

15283

15284

15285

15286

15287

15289

15290

15291

15292

15293

'%s' is not a valid value for @autofix. The only valid value is 'auto'.

Usage: sp_certify_removable <dbname> [,'auto']

The database that you are attempting to certify cannot be in use at the same time.

The database must be owned by a member of the sysadmin role before it can be removed.

Usage: sp_create_removable <dbname>,<syslogical>,<sysphysical>,<syssize>,<loglogical>,<logphysical>,<logsize>,<datalogical1>,<dataphysical1>,<datasize1> [,<datalogical2>,<dataphysical2>,<datasize2>...<datalogical16>,<dataphysical16>,<datasize16>]

Invalid file size entered. All files must be at least 1 MB.

Could not create the '%s' portion of the database.

Cannot make '%s' database removable.

Logical data device '%s' not created.

You cannot specify a length for user data types based on sysname.

Invalid @with_log parameter value. Valid values are 'true' or 'false'.

FOREIGN KEY constraints do not have space allocated.

The only valid @parameter_value values are 'true' or 'false'.

Login '%s' is already mapped to user '%s' in database '%s'.

You must add the us_english version of this message before you can add the '%s' version.

All localized versions of this message must be dropped before the us_english version can be dropped.

The name '%s' contains too many characters.

The user has granted or revoked privileges to the following in the database and cannot be dropped.

The special word '%s' cannot be used for a logical device name.

Terminating this procedure. The @action '%s' is unrecognized. Try 'REPORT', 'UPDATE_ONE', or 'AUTO_FIX'.

Terminating this procedure. '%s' is a forbidden value for the login name parameter in this procedure.

Terminating this procedure. Cannot have an open transaction when this is run.

Terminating this procedure. The Action '%s' is incompatible with the other parameter values ('%s', '%s').

Terminating this procedure. The %s name '%s' is absent or invalid.

The row for user '%s' will be fixed by updating its login link to a login already in existence.

Barring a conflict, the row for user '%s' will be fixed by updating its link to a new login. Consider changing the new password from null.

Description

15294

15295

15298

The number of orphaned users fixed by adding new logins and then updating users was %d.

The number of orphaned users fixed by updating users was %d.

New login created.

Table 3–77:TSQL Error Codes - 15300 to 15399

Description

15300

15301

15302

15303

15304

15305

15306

15307

15308

15311

15312

15318

15319

15321

15322

15323

15324

15325

15326

15327

15328

15330

No recognized letter is contained in the parameter value for General Permission Type (%s). Valid letters are in this set: %s .

Collation '%s' is supported for Unicode data types only and cannot be set at either the database or server level.

Database_Name should not be used to qualify owner.object for the parameter into this procedure.

The "user options" config value (%d) was rejected because it would set incompatible options.

The severity level of the '%s' version of this message must be the same as the severity level (%ld) of the us_english version.

The @TriggerType parameter value must be 'insert', 'update', or 'delete'.

Cannot change the compatibility level of replicated or distributed databases.

Could not change the merge publish option because the server is not set up for replication.

You must set database '%s' to single user mode with sp_dboption before fixing indexes on system tables.

The file named '%s' does not exist.

The file named '%s' is a primary file and cannot be removed.

All fragments for database '%s' on device '%s' are now dedicated for log usage only.

Error: DBCC DBREPAIR REMAP failed for database '%s' (device '%s').

There was some problem removing '%s' from sysaltfiles.

File '%s' was removed from tempdb, and will take effect upon server restart.

The selected index does not exist on table '%s'.

The option %s cannot be changed for the '%s' database.

The current database does not contain a %s named '%ls'.

No extended stored procedures exist.

The database is now offline.

The database is offline already.

There are no matching rows on which to report.

Description

15331

15333

15335

15336

15337

15338

15339

15340

15341

15354

15358

15363

15379

15387

15388

15390

15394

15395

15398

15399

The user '%s' cannot take the action auto_fix due to duplicate SID.

Error: The qualified @oldname references a database (%s) other than the current database.

Error: The @newname value '%s' is already in use as a %s name and would cause a duplicate that is not permitted.

Object '%s' cannot be renamed because the object participates in enforced dependencies.

Caution: sysdepends shows that other objects (views, procedures and so on) are referencing this object by its old name. These objects will become invalid, and should be dropped and re-created promptly.

The %s was renamed to '%s'.

Creating '%s'.

Alias user added.

Granted database access to '%s'.

Usage: sp_detach_db <dbname>, [TRUE|FALSE]

User-defined filegroups should be made read-only.

The role '%s' already exists in the current database.

The server option value '%s' supplied is unrecognized.

If the qualified object name specifies a database, that database must be the current database.

There is no user table matching the input name '%s' in the current database.

Input name '%s' does not have a matching user table or indexed view in the current database.

Collation '%s' is not supported by the operating system

The qualified old name could not be found for item type '%s'.

Only objects in the master database owned by dbo can have the startup setting changed.

Could not change startup option because this option is restricted to objects that have no parameters.

Table 3–78:TSQL Error Codes - 15400 to 15499

Description

15401

15402

15405

15407

15409

Windows NT user or group '%s' not found. Check the name again.

'%s' is not a fixed server role.

Cannot use the reserved user or role name '%s'.

'%s' is not a valid Windows NT name. Give the complete name:
<domain\username>.

'%s' is not a role.

Description

15410

15412

15413

15414

15415

15416

15417

15418

15419

15420

15421

15422

15423

15424

15425

15426

15427

15428

15429

15430

15431

15432

15433

15434

15435

15436

15437

15438

15439

15440

15441

15442

User or role '%s' does not exist in this database.

'%s' is not a known fixed role.

Cannot make a role a member of itself.

Cannot set compatibility level because database has a view or computed column that is indexed. These indexes require a SQL Server compatible database.

User is a member of more than one group. sp_changegroup is set up for backward compatibility and expects membership in one group at most.

Usage: sp_dbcmptlevel [dbname [, compatibilitylevel]]

Cannot change the compatibility level of the '%s' database.

Only members of the sysadmin role or the database owner may set the database compatibility level.

Supplied parameter @sid should be binary(16).

The group '%s' does not exist in this database.

The user owns role(s) in the database and cannot be dropped.

Application roles can only be activated at the ad hoc level.

The password for application role '%s' has been changed.

New role added.

New application role added.

You must specify a provider name with this set of properties.

You must specify a provider name for unknown product '%ls'.

You cannot specify a provider or any properties for product '%ls'.

'%ls' is an invalid product name.

Limit exceeded for number of servers.

You must specify the @rolename parameter.

Stored procedure '%s' can only be executed at the ad hoc level.

Supplied parameter @sid is in use.

Could not drop login '%s' as the user is currently logged in.

Database successfully published.

Database successfully enabled for subscriptions.

Database successfully published using merge replication.

Database is already online.

Database is now online.

Database is no longer published.

Database is no longer enabled for subscriptions.

Database is no longer enabled for merge publications.

Description

15443

15444

15445

15446

15447

15449

15450

15452

15453

15454

15456

15457

15458

15459

15460

15461

15462

15463

15467

15469

15470

15471

15472

15473

15475

15476

15477

15478

15479

15480

15481

15482

Checkpointing database that was changed.

'Disk' device added.

'Diskette' device added.

'Tape' device added.

'Pipe' device added.

Type added.

New language inserted.

No alternate languages are available.

us_english is always available, even though it is not in syslanguages.

Language deleted.

Valid configuration options are:

Configuration option '%ls' changed from %ld to %ld. Run the RECONFIGURE statement to install.

Database removed.

In the current database, the specified object references the following:

In the current database, the specified object is referenced by the following:

Object does not reference any object, and no objects reference it.

File '%s' closed.

Device dropped.

Type has been dropped.

No constraints have been defined for this object.

No foreign keys reference this table.

The object comments have been encrypted.

The object does not have any indexes.

Settable remote login options.

The database is renamed and in single user mode.

A member of the sysadmin role must reset the database to multiuser mode with sp_dboption.

Caution: Changing any part of an object name could break scripts and stored procedures.

Password changed.

Login dropped.

Could not grant login access to '%s'.

Granted login access to '%s'.

Could not deny login access to '%s'.

Description

15483

15484

15485

15486

15487

15488

15489

15490

15491

15492

15493

15494

15495

15496

15497

15498

15499

Denied login access to '%s'.

Could not revoke login access from '%s'.

Revoked login access from '%s'.

Default database changed.

%s's default language is changed to %s.

'%s' added to role '%s'.

'%s' dropped from role '%s'.

The dependent aliases were also dropped.

User has been dropped from current database.

Alias user dropped.

Role dropped.

The application role '%s' is now active.

Application role dropped.

Group changed.

Could not add login using sp_addlogin (user = %s). Terminating this procedure.

Inside txn_1a_, update failed. Will roll back (1a1).

The dependent aliases were mapped to the new database owner.

Table 3–79:TSQL Error Codes - 15500 to 15599

Description

15500

15501

15502

15503

15504

15505

15511

15512

15513

15514

15515

15516

15519

15520

The dependent aliases were dropped.

Database owner changed.

Setting database owner to SA.

Giving ownership of all objects to the database owner.

Deleting users except guest and the database owner from sysusers.

Cannot change owner of object '%ls' or one of its child objects because the new owner '%ls' already has an object with the same name.

Default bound to column.

Default bound to data type.

The new default has been bound to columns(s) of the specified user data type.

Rule bound to table column.

Rule bound to data type.

The new rule has been bound to column(s) of the specified user data type.

Default unbound from table column.

Default unbound from data type.

Description

15521

15522

15523

15524

15525

15526

15527

15528

15536

15543

15544

15545

15546

15547

15548

15549

15550

15551

15552

15553

15554

15555

15564

15565

15566

15567

15568

15569

15570

15571

15572

15573

15574

Columns of the specified user data type had their defaults unbound.

Rule unbound from table column.

Rule unbound from data type.

Columns of the specified user data type had their rules unbound.

sp_checknames is used to search for non 7-bit ASCII characters.

in several important columns of system tables. The following

columns are searched:

In master:

In all databases:

Looking for non 7-bit ASCII characters in the system tables of database '%s'.

Table.column '%s'

The following database names contain non 7-bit ASCII characters.

If you wish to change these names, use '%s'.

The following logins have default database names that contain

non 7-bit ASCII characters. If you wish to change these names use

sp_defaultdb.

The following servers have 'initialization file' names that contain

non 7-bit ASCII characters. If you wish to change these names,

use UPDATE.

Database '%s' has no object, user, and so on

names that contain non 7-bit ASCII characters.

The database name provided '%s' must be the current database when executing this stored procedure.

The following device names contain non 7-bit ASCII characters.

The following login names contain non 7-bit ASCII characters.

The following remote login names contain non 7-bit ASCII characters.

The following server names contain non 7-bit ASCII characters.

The following column and parameter names contain non 7-bit ASCII characters.

The following index names contain non 7-bit ASCII characters.

The following object names contain non 7-bit ASCII characters.

The following segment names contain non 7-bit ASCII characters.

The following data type names contain non 7-bit ASCII characters.

The following user or role names contain non 7-bit ASCII characters.

This object does not have any statistics.

Description

15575

15576

15577

This object does not have any statistics or indexes.

You cannot set network name on server '%ls' because it is not a linked SQL
Server.

Warning: A linked server that refers to the originating server is not a supported scenario. If you wish to use a four-part name to reference a local table, please use the actual server name rather than an alias.

Table 3–80:TSQL Error Codes - 15600 to 15699

Description

15600

15601

15604

15605

15606

15607

15608

15609

15610

15611

15612

15613

15614

15615

15616

15617

15618

15619

15620

15621

15622

15623

15624

An invalid parameter or option was specified for procedure '%s'.

Full-Text Search is not enabled for the current database. Use sp_fulltext_database to enable Full-Text Search.

Cannot drop full-text catalog '%ls' because it contains a full-text index.

A full-text index for table '%ls' has already been created.

You must first create a full-text index on table '%ls'.

'%ls' is not a valid index to enforce a full-text search key. You must specify a unique, non-nullable, single-column index.

Full-text search has already been activated for table '%ls'.

Cannot activate full-text search for table '%ls' because no columns have been enabled for full-text search.

You must deactivate full-text search on table '%ls' before adding columns to or removing columns from the full-text index.

Column '%ls' of table '%ls' cannot be used for full-text search because it is not a character-based column.

DBCC DBCONTROL error. Database was not made read-only.

The database is now read-only.

The database already is read-only.

DBCC DBCONTROL error. Database was not made single user.

The database is now single user.

The database already is single user.

The database is now read/write.

The database already is read/write.

The database is now multiuser.

The database already is multiuser.

No permission to access database '%s'.

Enabling %ls option for database '%ls'.

Disabling %ls option for database '%ls'.

Description

15625

15626

15627

15630

15631

15632

15633

15634

15635

15636

15637

15638

15639

15640

15642

15643

15644

15645

15646

15647

Option '%ls' not recognized for '%ls' parameter.

You attempted to acquire a transactional application lock without an active transaction.

sp_dboption command failed.

Full-text search must be activated on table '%ls' before this operation can be performed.

Full-text change tracking is currently enabled for table '%ls'.

Full-text change tracking must be started on table '%ls' before full-text auto propagation can begin.

Full-text auto propagation is currently enabled for table '%ls'.

Full-text change tracking must be started on table '%ls' before the changes can be flushed.

Cannot execute '%ls' because the database is in read-only access mode.

Full-text catalog '%ls' cannot be populated because the database is in single-user access mode.

Full-text index for table '%ls' cannot be populated because the database is in single-user access mode.

Warning: Full-text index for table '%ls' cannot be populated because the database is in single-user access mode. Change tracking is stopped for this table. Use sp_fulltext_table to start change tracking.

Warning: Table '%s' does not have the option 'text in row' enabled and has full-text indexed columns that are of type image, text, or ntext. Full-text change tracking cannot track WRITETEXT or UPDATETEXT operations performed on these columns.

sp_fulltext_table 'start_full' must be executed on table '%ls'. Columns affecting the index have been added or dropped since the last index full population.

The ongoing population is necessary to ensure an up-to-date index. If needed, stop change tracking, and then deactivate the full-text index population.

Warning: This operation did not succeed on one or more tables. A table may be inactive, or a full-text index population may already be active.

Full-text index population failed to start on this table. Execute sp_fulltext_table '%ls', '%ls' to update the index.

Column '%ls' does not exist.

Column '%ls' is not a computed column.

No views with schema binding reference this table.

Table 3–81:TSQL Error Codes - 16800 to 16899

Description

16801

sp_dropwebtask requires at least one defined parameter @outputfile or @procname.

Description

16802

16803

16804

16805

16806

16807

16808

16809

16810

16811

16812

16813

16814

16815

16816

16817

16820

16821

16822

16823

16824

16825

16826

16827

16828

16829

16830

16831

16832

sp_dropwebtask cannot find the specified task.

sp_runwebtask requires at least one defined parameter @outputfile or @procname.

SQL Web Assistant: Could not establish a local connection to SQL Server.

SQL Web Assistant: Could not execute the SQL statement.

SQL Web Assistant: Could not bind the parameter to the SQL statement.

SQL Web Assistant: Could not obtain a bind token.

SQL Web Assistant: Could not find the existing trigger. This could be due to encryption.

SQL Web Assistant failed on the call to SQLGetData.

SQL Web Assistant failed on the call to SQLFetch.

SQL Web Assistant failed to bind a results column.

SQL Web Assistant: The @query parameter must be specified.

SQL Web Assistant: Parameters can be passed either by name or position.

SQL Web Assistant: Invalid parameter.

SQL Web Assistant: @procname is not valid.

SQL Web Assistant: @outputfile is not valid.

SQL Web Assistant: Could not read the given file.

SQL Web Assistant failed because the state of the Web task in msdb..MSwebtasks is invalid.

SQL Web Assistant: Could not open the output file.

SQL Web Assistant: Could not open the template file.

SQL Web Assistant: Could not allocate enough memory to satisfy this request.

SQL Web Assistant: The template file specified in the Web task has a bad size.

SQL Web Assistant: Could not read the template file.

SQL Web Assistant: Could not find the specified marker for data insertion in the template file.

SQL Web Assistant: Could not write to the output file.

SQL Web Assistant: @tabborder must be tinyint.

SQL Web Assistant: @singlerow must be 0 or 1. Cannot specify this parameter with @nrowsperpage.

SQL Web Assistant: The @blobfmt parameter specification is invalid.

SQL Web Assistant: The output file name is mandatory for every column specified in the @blobfmt parameter.

SQL Web Assistant: Procedure called with too many parameters.

Description

16833

16834

16838

16839

16841

16842

16843

16844

16845

16846

16847

16848

16849

16850

16851

16852

16853

16854

16855

16856

16857

16858

16859

16860

16861

16862

16863

16864

SQL Web Assistant: @nrowsperpage must be a positive number and it cannot be used with @singlerow.

SQL Web Assistant: Read/write operation on text, ntext, or image column failed.

SQL Web Assistant: Could not find the table in the HTML file.

SQL Web Assistant: Could not find the matching end table tag in the HTML file.

SQL Web Assistant: The @datachg parameter cannot be specified with the given @whentype value.

SQL Web Assistant: Could not find and drop the necessary trigger for updating the Web page.

SQL Web Assistant: Could not add the necessary trigger for the @datachg parameter. There could be an existing trigger on the table with missing or encrypted text.

SQL Web Assistant: Incorrect syntax for the @datachg parameter.

SQL Web Assistant: @datachg must be specified for the given @whentype option.

SQL Web Assistant: @unittype and/or @numunits must be specified for the given @whentype option.

SQL Web Assistant: @fixedfont must be 0 or 1.

SQL Web Assistant: @bold must be 0 or 1.

SQL Web Assistant: @italic must be 0 or 1.

SQL Web Assistant: @colheaders must be 0 or 1.

SQL Web Assistant: @lastupdated must be 0 or 1.

SQL Web Assistant: @HTMLheader must be in the range 1 to 6.

SQL Web Assistant: @username is not valid.

SQL Web Assistant: @dbname is not valid.

SQL Web Assistant: @whentype must be in the range 1 to 9.

SQL Web Assistant: @unittype must be in the range 1 to 4.

SQL Web Assistant: @targetdate is invalid. It must be a valid date after 1900-01-01.

SQL Web Assistant: The @targettime parameter must be between 0 and 240000.

SQL Web Assistant: @dayflags must be 1, 2, 4, 8, 16, 32, or 64.

SQL Web Assistant: @numunits must be greater than 0.

SQL Web Assistant: @targetdate must be specified for the given @whentype option.

SQL Web Assistant: @dayflags must be specified for the given @whentype option.

SQL Web Assistant: URL specification is invalid.

SQL Web Assistant: @blobfmt is invalid. The file must include the full path to the output_file location.

Description

16865

16866

16867

16868

16869

16870

16871

16873

16874

16875

16876

16877

16878

16879

16880

16881

16882

16883

16884

16885

16886

16887

16888

16889

16890

SQL Web Assistant: URL hyperlink text column must not be of the image data type.

SQL Web Assistant: Could not obtain the number of columns in @query.

SQL Web Assistant: URL hyperlink text column is missing in @query.

SQL Web Assistant failed on the call to SQLColAttribute.

SQL Web Assistant: Columns of data type image cannot have a template.

SQL Web Assistant: Internal error. Could not read @ parameters.

SQL Web Assistant: Invalid @charset. Execute sp_enumcodepages for a list of character sets.

SQL Web Assistant: Invalid @codepage. Execute sp_enumcodepages for a list of code pages.

SQL Web Assistant: Internal error. Cannot translate to the specified code page.

SQL Web Assistant: Translation to the desired code page is unavailable on this system.

SQL Web Assistant: Internal error. Could not obtain COM interface ID.

SQL Web Assistant: Internal error. Could not obtain COM language ID.

SQL Web Assistant: Internal error. Could not initialize COM library.

SQL Web Assistant: Internal error. Could not translate from Unicode to the specified code page.

SQL Web Assistant: Internal error. Could not create translation object. Make sure that the file MLang.dll is in your system directory.

SQL Web Assistant: This version is not supported on Win32s of Windows 3.1.

SQL Web Assistant: Web task not found. Verify the name of the task for possible errors.

SQL Web Assistant: Could not list Web task parameters. xp_readwebtask requires @procname.

SQL Web Assistant: Procedure name is required to convert Web tasks.

SQL Web Assistant: Could not upgrade the Web task to 7.0. The Web task will remain in 6.5 format and will need to be re-created.

SQL Web Assistant: Could not update Web tasks system table. The Web task remains in 6.5 format.

SQL Web Assistant: @procname parameter is missing.The parameter is required to upgrade a Web task to 7.0.

SQL Web Assistant: Source code page is not supported on the system. Ensure @charset and @codepage language files are installed on your system.

SQL Web Assistant: Could not send Web task row to the client.

SQL Web Assistant: ODS error occurred. Could not send Web task parameters.

Table 3–82:TSQL Error Codes - 16900 to 16999

Description

16901

16902

16903

16904

16905

16907

16909

16911

16914

16915

16916

16917

16922

16924

16925

16926

16927

16929

16930

16931

16932

16933

16934

16935

16936

16937

16938

%hs: This feature has not been implemented yet.

%hs: The value of parameter %hs is invalid.

%hs procedure called with incorrect number of parameters.

sp_cursor: optype:You can only specify ABSOLUTE in conjunction with DELETE or UPDATE.

The cursor is already open.

%hs is not allowed in cursor statements.

%hs: The cursor identifier value provided (%x) is not valid.

%hs: The fetch type %hs cannot be used with forward only cursors.

%hs procedure called with too many parameters.

A cursor with the name '%.*ls' already exists.

A cursor with the name '%.*ls' does not exist.

Cursor is not open.

Cursor Fetch: Implicit conversion from data type %s to %s is not allowed.

Cursorfetch: The number of variables declared in the INTO list must match that of selected columns.

The fetch type %hs cannot be used with dynamic cursors.

sp_cursoroption: The column ID (%d) does not correspond to a text, ntext, or image column.

Cannot fetch into text, ntext, and image variables.

The cursor is READ ONLY.

The requested row is not in the fetch buffer.

There are no rows in the current fetch buffer.

The cursor has a FOR UPDATE list and the requested column to be updated is not in this list.

The cursor does not include the table being modified or the table is not updatable through the cursor.

Optimistic concurrency check failed. The row was modified outside of this cursor.

No parameter values were specified for the sp_cursor-%hs statement.

sp_cursor: One or more values parameters were invalid.

A server cursor is not allowed on a remote stored procedure or stored procedure with more than one SELECT statement. Use a default result set or client cursor.

sp_cursoropen/sp_cursorprepare: The statement parameter can only be a single select or a single stored procedure.

Description

16940

16941

16942

16943

16944

16945

16946

16947

16948

16949

16950

16951

16952

16953

16954

16955

16956

16957

16958

16959

16960

16961

16962

16963

16996

16998

16999

Cannot specify UPDLOCK or TABLOCKX with READ ONLY or INSENSITIVE cursors.

Cursor updates are not allowed on tables opened with the NOLOCK option.

Could not generate asynchronous keyset. The cursor has been deallocated.

Could not complete cursor operation because the table schema changed after the cursor was declared.

Cannot specify UPDLOCK or TABLOCKX on a read-only table in a cursor.

The cursor was not declared.

Could not open the cursor because one or more of its tables have gone out of scope.

No rows were updated or deleted.

The variable '%.*ls' is not a cursor variable, but it is used in a place where a cursor variable is expected.

The variable '%.*ls' is a cursor variable, but it is used in a place where a cursor variable is not valid.

The variable '%.*ls' does not currently have a cursor allocated to it.

The variable '%.*ls' cannot be used as a parameter because a CURSOR OUTPUT parameter must not have a cursor allocated to it before execution of the procedure.

A cursor variable cannot be used as a parameter to a remote procedure call.

Remote tables are not updatable. Updatable keyset-driven cursors on remote tables require a transaction with the REPEATABLE_READ or SERIALIZABLE isolation level spanning the cursor.

Executing SQL directly; no cursor.

Could not create an acceptable cursor.

Cursor created was not of the requested type.

FOR UPDATE cannot be specified on a READ ONLY cursor.

Could not complete cursor operation because the set options have changed since the cursor was declared.

Unique table computation failed.

You have reached the maximum number of cursors allowed.

One or more FOR UPDATE columns have been adjusted to the first instance of their table in the query.

The target object type is not updatable through a cursor.

You cannot specify scroll locking on a cursor that contains a remote table.

%hs cannot take output parameters.

Internal Cursor Error: A cursor work table operation failed.

Internal Cursor Error: The cursor is in an invalid state.

Table 3–83:TSQL Error Codes - 17000 to 17099

Description

17000

17050

17052

17053

17054

17055

17059

17065

17066

17067

17068

Usage: sp_autostats <table_name> [, {ON|OFF} [, <index_name>] ]

The '%ls' option is ignored in this edition of SQL Server.

%1

%1: Operating system error %2 encountered.

LogEvent: Failed to report the current event. Operating system error = %1.

%1 :%n%2

Operating system error %1!d!: %2!hs!.

SQL Server Assertion: File: <%1>, line = %2!d! %nFailed Assertion = '%3' %4.

SQL Server Assertion: File: <%1>, line=%2!d! %nFailed Assertion = '%3'.

SQL Server Assertion: File: <%1>, line = %2!d! %n%3.

PrintStack Request

Table 3–84:TSQL Error Codes - 17100 to 17199

Description

17104

17112

17113

17114

17117

17118

17119

17120

17122

17124

17125

17126

17127

17128

17130

17131

17132

17134

17138

Server Process ID is %1!ld!.

Invalid command option %1!c!.

initconfig: Error %2 opening '%1' for configuration information.

initconfig: Error %2 reading configuration information from '%1'.

initconfig: Number of user connections reduced to %1!ld!.

upinit: Warning: Could not raise priority of %1 thread.

initconfig: Number of server processes reduced to %1!ld!.

SQL Server could not spawn %1 thread.

initdata: Warning: Could not set working set size to %1!d! KB.

SQL Server configured for %1 mode processing.

Using %1 lock allocation. [%2!d!] Lock Blocks, [%3!d!] Lock Owner Blocks.

SQL Server is ready for client connections

initdata: No memory for kernel buffer hash table.

initdata: No memory for kernel buffers.

initdata: No memory for kernel locks.

initdata: Not enough memory for descriptor hash tables.

initdata: Not enough memory for descriptors.

initmaster: Could not allocate process status structure (PSS).

Could not allocate enough memory to initialize '%1'.

Description

17140

17141

17142

17143

17144

17145

17146

17147

17148

17151

17154

17156

17157

17160

17161

17162

17168

Could not dispatch SQL Server by Service Control Manager. Operating system error = %1.

Could not register Service Control Handler. Operating system error = %1.

SQL Server has been paused. No new connections will be allowed.

%1: Could not set Service Control Status. Operating system error = %2.

SQL Server is disallowing new connections due to 'pause' request from Service
Control Manager.

Service Control Handler received an invalid control code = %1!d!.

SQL Server is allowing new connections due to 'continue' request from Service
Control Manager.

SQL Server terminating because of system shutdown.

SQL Server is terminating due to 'stop' request from Service Control Manager.

Maximum number of pages in batch I/O is limited to %1!ld!.

initdata: Not enough memory for procedure cache/hash table.

initeventlog: Could not initiate the EventLog Service for the key '%1'.

%1: Could not initialize Communication Layer.

Could not use SQLEVN70.DLL version '%1'. SQLEVN70.DLL version '%2' was expected.

Master device sector size is %1!d!. SQL Server cannot use the NO_BUFFERING option during I/O.

SQL Server is starting at priority class '%1'(%2!d! %3 detected).

SQL Server shut down because configured codepage %1!d! is not supported by the

Table 3–85:TSQL Error Codes - 17200 to 17299

Description

17204

17207

17208

17218

17249

17252

17253

17254

%1: Could not open device %2 for virtual device number (VDN) %3!d!.

%1: Operating system error %3 during the creation/opening of physical device %2.

%1: File '%2' has an incorrect size (%3!d! MB, should be %4!d! MB).

%1: Operating system error %2 on device '%3' (virtual page %4).

%1: Negative outstanding I/O count in process ID = %2!d!.

%1: Actual bytes transferred (%2!d!) does not match requested amount (%3!d!) on device '%4' (virtual page %5).

The sector size for device %1 is %2!d!. SQL Server cannot use the NO_BUFFERING option during I/O on this device.

Warning: Cannot use NO_BUFFERING option on '%1'. Operating system error %2.

Table 3–86:TSQL Error Codes - 17300 to 17399

Description

17300

17302

17303

17304

17308

17309

17310

17311

Not enough memory for process status structure (PSS) allocation.

The maximum limit for connections has been reached.

freepss: Bad process status structure (PSS) value.

Warning: Clean_process system function called from another thread. Outstanding I/O may not complete.

%1: Process %2!d! generated an access violation. SQL Server is terminating this process.

The current contents of process' input buffer are '%1'.

%1: Process %2!d! generated fatal exception %3!lx! %4. SQL Server is terminating this process.

SQL Server is aborting. Fatal exception %1!lx! caught.

Table 3–87:TSQL Error Codes - 17400 to 17499

Description

17402

17422

17423

17424

17426

17429

17430

Database '%1' set to single user mode.

closetable: Called with null session descriptor (SDES), server process ID (SPID) %1!d!.

closetable: Table already closed for session descriptor (SDES) %1!08lx!.

Warning: OPEN OBJECTS parameter may be too low.

Run sp_configure to increase the parameter value.

The srchindex system function failed for index ID = %1!d!, sridoff = %2!d!.

Database '%1' set to read only mode.

Table 3–88:TSQL Error Codes - 17500 to 17599

Description

17550

17551

17557

17558

17560

17561

17569

17570

17571

17572

DBCC TRACEON %d, server process ID (SPID) %d.

DBCC TRACEOFF %d, server process ID (SPID) %d.

DBCC DBRECOVER failed for database ID %d.

*** Bypassing recovery for database ID %d.

DBCC DBREPAIR: '%ls' index restored for '%ls.%ls'.

%ls index restored for %ls.%ls.

DBCC cannot find the library initialization function %ls.

DBCC cannot find the function %ls in the library %ls.

DBCC function %ls in the library %ls generated an access violation. SQL Server is terminating process %d.

DBCC cannot free DLL %ls. SQL Server depends on this DLL to function properly.

Table 3–89:TSQL Error Codes - 17600 to 17699

Description

17654

17657

17658

17660

17661

17669

17674

17676

Warning: Process status structure (PSS) found with open session descriptor (SDES). PSPID %1!d!, PSUID %2!d!, PCURDB %3!d!, range entry %4!d!, SDESP 0x%5!lx!, object ID %6!ld!.

Attempting to change default collation to %1.

SQL Server started in single user mode. Updates allowed to system catalogs.

Starting without recovery.

Recovering all databases but not clearing tempdb.

Table still open. Database ID %1!d!, table ID %2!ld!.

Login: %1 %2, server process ID (SPID): %3!d!, kernel process ID (KPID): %4!d!.

SQL Server shutdown due to Ctrl-C or Ctrl-Break signal.

Table 3–90:TSQL Error Codes - 17700 to 17799

Description

17750

17751

17752

17753

Cannot load the DLL %ls, or one of the DLLs it references. Reason: %ls.

Cannot find the function %ls in the library %ls. Reason: %ls.

Extended procedure memory allocation failed for '%ls'.

%.*ls can only be executed in the master database.

Table 3–91:TSQL Error Codes - 17800 to 17899

Description

17801

17803

17804

17805

17807

17808

17809

17814

17815

17820

17822

17824

17825

17826

17831

17832

17833

17834

17837

17838

17839

17840

17841

17842

17843

17844

17845

17846

17847

17848

Unknown internal error value.

Insufficient memory available.

Invalid 'nbytes' value.

Invalid buffer received from client.

Invalid event '%1!ld!'.

Invalid starting position specified.

Could not connect. The maximum number of '%1!ld!' configured user connections are already connected. The system administrator can change the maximum to a higher value using sp_configure.

Invalid function parameter.

No longer waiting for client connections using Net-Library'%1!hs!'.

Invalid data type parameter.

Could not load Net-Library '%1!hs!'.

Could not write to Net-Library '%1!hs!', loginname '%2!ls!', hostname '%3!ls!'. Connection closed.

Could not close Net-Library '%1!hs!'.

Could not set up Net-Library '%1!hs!'.

Could not load Net-Library '%1!hs!' version '%2!hs!'. Need Net-Library version '%3!hs!' or greater.

Connection opened but invalid login packet(s) sent. Connection closed.

Net-Library %1!hs!' is already in use.

Using '%1!hs!' version '%2!hs!'.

char data type%0

variable-length char data type%0

binary data type%0

variable-length binary data type%0

1-byte integer data type%0

2-byte integer data type%0

4-byte integer data type%0

bit data type%0

datetime data type%0

datetime data type, nulls allowed%0

money data type%0

money data type, nulls allowed%0

Description

17849

17850

17851

17852

17853

17854

17855

17856

17857

17858

17859

17868

17869

17870

17871

17872

17873

17874

17875

17876

17877

17878

17879

17880

17881

17882

4-byte float data type, nulls allowed%0

8-byte float data type%0

8-byte float data type, nulls allowed%0

4-byte datetime data type, nulls allowed%0

4-byte money data type%0

event type%0

done packet status field%0

error severity type%0

4-byte integer data type, nulls allowed%0

image data type%0

text data type%0

numeric data type%0

numeric data type, nulls allowed%0

decimal data type%0

decimal data type, nulls allowed%0

bit data type, nulls allowed%0

8000-byte variable-length binary data type%0

8000-byte variable-length character data type%0

8000-byte binary data type%0

8000-byte character data type%0

8000-byte Unicode character data type%0

8000-byte Unicode variable-length character data type%0

Unicode text data type%0

uniqueidentifier data type%0

'%1!ls!' is an unsupported Open Data Services API.

Error accepting connection request via Net-Library '%1!hs!'. Execution continuing.

Table 3–92:TSQL Error Codes - 18000 to 18099

Description

18002

18052

18053

Stored function '%.*ls' in the library '%.*ls' generated an access violation. SQL Server is terminating process %d.

Error: %1!d!, Severity: %2!d!, State: %3!d!.

Error: %1!d!, Severity: %2!d!, State: %3!d!%n%4%5.

Table 3–93:TSQL Error Codes - 18100 to 18199

Description

18100

18113

18124

Process ID %d killed by hostname %.*ls, host process ID %d.

SQL Server shutdown after verifying system indexes.

Default collation successfully changed.

Table 3–94:TSQL Error Codes - 18200 to 18299

Description

18200

18201

18203

18204

18205

18207

18208

18209

18210

18211

18213

18214

18215

18216

18217

18218

18219

18221

18223

18225

18227

18257

18264

18265

%1: Backup device ID %2!d! out of range.

ksconsole: Cannot create ConsBufMutex: %1.

ksconsole: Cannot create %1 : %2.

%1: Backup device '%2' failed to %3. Operating system error = %4.

%1: Could not initialize console operation.

%1: Null request packet.

%1: Backup device ID %2!d! is not active.

ksconsole: Could not send request to console client.

%1: %2 failure on backup device '%3'. Operating system error %4.

ksconsole: Could not receive request from console client.

ksconsole: Console input request for type 0x%1!x!, ID 0x%2!x! failed.

%1: Server console thread not running.

%1: Response type 0x%2!x!, ID 0x%3!x! not found in request.

%1: Could not access console mutex. Operating system error %2.

%1: Type 0x%2!x! not implemented.

%1: Incorrect number of parameters: %2!d!.

ksconsole: Could not close console connection.

ksconsole: Reinitializing the console.

%1: No console client connected. Start CONSOLE.EXE.

Tape '%1' (Family ID: %2, sequence %3) mounted on tape drive '%4'.

Unnamed tape (Family ID: %1, sequence %2) mounted on tape drive '%3'.

%1: Device or media does not support %2.

Database backed up: Database: %1, creation date(time): %2(%3), pages dumped:
%4!d!, first LSN: %5, last LSN: %6, number of dump devices: %9!d!, device information: (%10).

Log backed up: Database: %1, creation date(time): %2(%3), first LSN: %4, last LSN: %5, number of dump devices: %7!d!, device information: (%8).

Description

18266

18267

18268

18269

18270

18271

18272

18273

18274

18275

18276

18277

18278

Database file backed up: Database: %1, creation date(time): %2(%3), file list:
(%4), pages dumped: %5!d!, number of dump devices: %8!d!, device information:
(%9).

Database restored: Database: %1, creation date(time): %2(%3), first LSN: %4, last LSN: %5, number of dump devices: %7!d!, device information: (%8).

Log restored: Database: %1, creation date(time): %2(%3), first LSN: %4, last LSN: %5, number of dump devices: %7!d!, device information: (%8).

Database file restored: Database: %1, creation date(time): %2(%3), file list: (%4), number of dump devices: %6!d!, device information: (%7).

Database differential changes backed up: Database: %1, creation date(time):
%2(%3), pages dumped: %4!d!, first LSN: %5, last LSN: %6, full backup LSN:
%7, number of dump devices: %10!d!, device information: (%11).

Database changes restored: Database: %1, creation date(time): %2(%3), first
LSN: %4, last LSN: %5, number of dump devices: %7!d!, device information:
(%8).

I/O error on backup or restore restart-checkpoint file '%1'. Operating system error %2. The statement is proceeding but is non-restartable.

Could not clear '%1' bitmap in database '%2' due to error %3!d!. A subsequent backup operation may be slower/larger than normal.

Tape '%1' (Family ID: %2, sequence %3) dismounted from tape drive '%4'.

Unnamed tape (Family ID: %1, sequence %2) dismounted from tape drive '%3'.

Database file differential changes backed up: Database: %1, creation date(time):
%2(%3), file list: (%4), pages dumped: %5!d!, number of dump devices: %8!d!, device information: (%9).

Database file changes restored: Database: %1, creation date(time): %2(%3), file list: (%4), number of dump devices: %6!d!, device information: (%7).

Database log truncated: Database: %1.

Table 3–95:TSQL Error Codes - 18400 to 18499

Description

18400

18450

18451

18452

18453

18454

18455

18456

Checkpoint process is terminating due to a fatal exception.

Login failed for user '%ls'. Reason: Not defined as a valid user of a trusted SQL Server connection.

Login failed for user '%ls'. Only administrators may connect at this time.

Login failed for user '%ls'. Reason: Not associated with a trusted SQL Server connection.

Login succeeded for user '%ls'. Connection: Trusted.

Login succeeded for user '%ls'. Connection: Non-Trusted.

Login succeeded for user '%ls'.

Login failed for user '%ls'.

Description

18457

18458

18459

18460

18461

18482

18483

18485

18490

18491

18492

Login failed for user '%ls'. Reason: User name contains a mapping character or is longer than 30 characters.

Login failed. The maximum simultaneous user count of %d licenses for this server has been exceeded. Additional licenses should be obtained and registered through the Licensing application in the Windows NT Control Panel.

Login failed. The maximum workstation licensing limit for SQL Server access has been exceeded.

Login failed. The maximum simultaneous user count of %d licenses for this '%ls' server has been exceeded. Additional licenses should be obtained and installed or you should upgrade to a full version.

Login failed for user '%ls'. Reason: Server is in single user mode. Only one administrator can connect at this time.

Could not connect to server '%ls' because '%ls' is not defined as a remote server.

Could not connect to server '%ls' because '%ls' is not defined as a remote login at the server.

Could not connect to server '%ls' because it is not configured for remote access.

Maximum number of processors supported is '%1!ld!'.

Could not start due to invalid serial number.

The license agreement has been violated for this '%1' version of SQL Server. Cannot start.

Table 3–96:TSQL Error Codes - 18500 to 18599

Description

18500

18501

18502

Could not load startup handler DLL '%1'.

Could not load startup handler function '%1'.

Could not add startup handler '%1'.

Table 3–97:TSQL Error Codes - 18600 to 18699

Description

18666

Could not free up descriptor in rel_desclosed() system function.

Table 3–98:TSQL Error Codes - 18700 to 18799

Description

18750

18751

18752

18754

18755

%ls: The parameter '%ls' is invalid.

%ls procedure called with incorrect number of parameters.

Another log reader is replicating the database.

Could not open table %d.

Could not allocate memory for replication.

Description

18756

18757

18759

18760

18761

18762

18763

18764

18765

18766

18767

18768

18769

18770

18771

18772

18773

18774

18775

18776

18777

18778

Could not get replication information for table %d.

The database is not published.

Replication failure. File '%ls', line %d.

Invalid %ls statement for article %d.

Commit record at (%ls) has already been distributed. Check DBTABLE.

Invalid begin LSN (%ls) for commit record (%ls). Check DBTABLE.

Commit record (%ls) reports oldest active LSN as (0:0:0).

Execution of filter stored procedure %d failed. See the SQL Server errorlog for more information.

Begin LSN specified for replication log scan is invalid.

The replbeginlsn field in the DBTABLE is invalid.

The specified begin LSN (%ls) for replication log scan occurs before replbeginlsn (%ls).

The specified LSN (%ls) for repldone log scan occurs before the current start of replication in the log (%ls).

The specified LSN (%ls) for repldone log scan is not a replicated commit record.

The specified LSN (%ls) for repldone log scan is not present in the transaction log.

Invalid storage type %d specified writing variant of type %d.

Invalid server data type (%d) specified in repl type lookup.

Could not locate text information records for column %d during command construction.

The stored procedure sp_replsetoriginator must be executed within a transaction.

The Log Reader Agent encountered an unexpected log record of type %u encountered while processing DML operation.

An error occurred while waiting on the article cache access event.

%s: Error initializing MSMQ components

%s: Error opening Microsoft Message Queue %s

Table 3–99:TSQL Error Codes - 18800 to 18899

Description

18800

18831

18833

Warning: Index '%1' on '%2' in database '%3' may be corrupt because of expression evaluation changes in this release. Drop and re-create the index.

;// Database ID %d. Could not find object descriptor for object ID %ld.

Database ID %d. Could not find clustered index on system table ID %ld. This index should exist in all databases. Run DBCC CHECKTABLE on sysindexes in the database.

Description

18836

18841

18843

18872

18874

18875

18876

18877

18883

18884

18885

18886

18887

18892

18894

18895

18901

Database ID %d. Could not find object ID %ld in sysobjects. This system catalog should exist in all databases. Run DBCC CHECKTABLE on sysobjects in this database.

Could not locate entry in sysdatabases for database '%.*ls'. No entry found with that name.

Could not find database ID %d in sysdatabases.

Rec_finish: getnext SCAN_NOINDEX on sysdatabases.dbid=%d failed.

Rec_complete: Could not open controlling database (ID %d) of controlling database in multi-database transaction

Recovering database '%.*s'.

%d transactions rolled forward in database '%.*ls' (%d).

%d transactions rolled back in database '%.*ls' (%d).

;//Database ID %d: Attempt to mark database SUSPECT. Getnext NC scan on
sysobjects.dbid failed.

;//Database '%.*s' (ID %d). Recovery failed. Run DBCC.

Page #%lx from table ID #%ld, database ID #%d, not found in cache.

Page #%lx from sysindexes in database ID #%X not in cache after reading it into cache.

Cannot recover the master database. Exiting.

Extent ID %ld which should belong to syslogs belongs to object ID %ld.

No more room in the transaction table.

Transaction (%d, %d) not found in the transaction table.

Could not build an allocation map for the database '%.*s'. Database does not have a DBINFO structure.

Table 3–100:TSQL Error Codes - 19000 to 19099

Description

19000

19001

19002

19003

19004

19010

19011

19012

19013

19014

19015

ODBC error encountered, State = %1, native error = %2, error message = %3.

Windows NT Error encountered, %1.

MS SQL SNMP Extension Agent starting, %1, version %2.

MS SQL SNMP Extension Agent reconnecting.

MS SQL SNMP Extension Agent stopping.

RPC Net-Library listening on: %1.

SuperSocket info: %1.

SuperSocket Info: Bind failed on TCP port %1.

SQL server listening on %1.

Invalid Protocol specified for a %1 instance: %2.

Encryption requested but no valid certificate was found. SQL Server terminating.

Table 3–101:TSQL Error Codes - 20000 to 20099

Description

20001

20002

20003

20007

20008

20009

20010

20011

20012

20013

20014

20015

20016

20017

20018

20019

There is no nickname for article '%s' in publication '%s'.

The filter '%s' already exists for article '%s' in publication '%s'.

Could not generate nickname for '%s'.

The system tables for merge replication could not be dropped successfully.

The system tables for merge replication could not be created successfully.

The article '%s' could not be added to the publication '%s'.

The Snapshot Agent corresponding to the publication '%s' could not be dropped.

Cannot set incompatible publication properties. The 'allow_anonymous' property of a publication depends on the 'immediate_sync' property.

The subscription type '%s' is not allowed on publication '%s'.

The publication property '%s' cannot be changed when there are subscriptions on it.

Invalid @schema_option value.

Could not remove directory '%ls'. Check the security context of xp_cmdshell and close other processes that may be accessing the directory.

Invalid @subscription_type value. Valid values are 'pull' or 'anonymous'.

The subscription on the Subscriber does not exist.

The @optional_command_line is too long. Use an agent definition file.

Replication database option '%s' cannot be set unless the database is a publishing database or a distribution database.

Description

20020

20021

20023

20025

20026

20027

20028

20029

20030

20031

20032

20033

20034

20036

20037

20038

20039

20040

20041

20043

20044

20045

20046

20047

20049

20050

20051

20054

20055

The article resolver supplied is either invalid or nonexistent.

The subscription could not be found.

Invalid @subscriber_type value. Valid options are 'local', 'global', 'anonymous', or 'repub'.

The publication name must be unique. The specified publication name '%s' has already been used.

The publication '%s' does not exist.

The article '%s' does not exist.

The Distributor has not been installed correctly. Could not enable database for publishing.

The Distributor has not been installed correctly. Could not disable database for publishing.

The article '%s' already exists on another publication with a different column tracking option.

Could not delete the row because it does not exist.

'%s' is not defined as a Subscriber for '%s'.

Invalid publication type.

Publication '%s' does not support '%s' subscriptions.

The Distributor has not been installed correctly.

The article '%s' already exists in another publication with a different article resolver.

The article filter could not be added to the article '%s' in the publication '%s'.

The article filter could not be dropped from the article '%s' in the publication '%s'.

Could not drop the article(s) from the publication '%s'.

Transaction rolled back. Could not execute trigger. Retry your transaction.

Could not change the article '%s' because the publication has already been activated.

The priority property is invalid for local subscribers.

You must supply an article name.

The article does not exist.

You are not authorized to perform this operation.

The priority value should not be larger than 100.0.

The retention period must be greater than or equal to %d.

The Subscriber is not registered.

Current database is not enabled for publishing.

Table '%s' cannot be published for merge replication because it has a timestamp column.

Description

20056

20057

20058

20059

20060

20061

20062

20064

20065

20066

20067

20068

20069

20070

20072

20073

20074

20075

20076

20077

20078

20079

20081

20084

20086

20087

20088

20089

20090

20091

Table '%s' cannot be republished.

The profile name '%s' already exists for the specified agent type.

The @agent_type must be 1 (Snapshot), 2 (Logreader), 3 (Distribution), or 4
(Merge)

The @profile_type must be 0 (System) or 1 (Custom)

Compatibility level cannot be smaller than 60.

The compatibility level of this database must be set to 70 or higher to be enabled for merge publishing.

Updating columns with the rowguidcol property is not allowed.

Cannot drop profile. Either it is not defined or it is defined as the default profile.

Cannot drop profile because it is in use.

Profile not defined.

The parameter name '%s' already exists for the specified profile.

The article cannot be created on table '%s' because it has more than %d columns.

Cannot validate a merge article that uses looping join filters.

Cannot update subscription row.

Cannot update Subscriber information row.

Articles can be added or changed only at the Publisher.

Only a table object can be published as a "table" article for merge replication.

The 'status' parameter value must be either 'active' or 'unsynced'.

The @sync_mode parameter value must be 'native' or 'character'.

Problem encountered generating replica nickname.

The @property parameter value must be 'sync_type', 'priority', or 'description'.

Invalid @subscription_type parameter value. Valid options are 'push', 'pull', or 'both'.

Publication property '%s' cannot be NULL.

Publication '%s' cannot be subscribed to by Subscriber database '%s'.

Publication '%s' does not support the nosync type because it contains a table that does not have a rowguidcol column.

You cannot push an anonymous subscription.

Only assign priorities that are greater than or equal to 0 and less than 100.

Could not get license information correctly.

Could not get version information correctly.

sp_mergesubscription_cleanup is used to clean up push subscriptions. Use sp_dropmergepullsubscription to clean up pull or anonymous subscriptions.

Description

20100

Cannot drop Subscriber '%s'. There are existing subscriptions.

Table 3–102:TSQL Error Codes - 20500 to 20599

Description

20500

20501

20502

20503

20505

20506

20507

20508

20509

20510

20511

20512

20515

20516

20517

20518

20519

20520

20521

20522

20523

20524

20525

The updatable Subscriber stored procedure '%s' does not exist in sysobjects.

Could not insert into sysarticleupdates using sp_articlecolumn.

Invalid '%s' value. Valid values are 'read only', 'sync tran', 'queued tran', or 'failover'.

Invalid '%s' value in '%s'. The publication is not enabled for '%s' updatable subscriptions.

Could not drop synchronous update stored procedure '%s' in '%s'.

Source table '%s' not found in '%s'.

Table '%s' not found in '%s'.

Updatable Subscriptions: The text/ntext/image values inserted at Subscriber will be NULL.

Updatable Subscriptions: The text/ntext/image values cannot be updated at
Subscriber.

Updatable Subscriptions: Cannot update identity columns.

Updatable Subscriptions: Cannot update timestamp columns.

Updatable Subscriptions: Rolling back transaction.

Updatable Subscriptions: Rows do not match between Publisher and Subscriber. Run the Distribution Agent to refresh rows at the Subscriber.

Updatable Subscriptions: Replicated data is not updatable.

Updatable Subscriptions: Update of replica's primary key is not allowed unless published table has a timestamp column.

Updatable Subscriptions: INSERT and DELETE operations are not supported unless published table has a timestamp column.

Updatable Subscriptions: INSERT operations on tables with identity or timestamp columns are not allowed unless a primary key is defined at the Subscriber.

Updatable Subscriptions: UPDATE operations on tables with identity or timestamp columns are not allowed unless a primary key is defined at the Subscriber.

sp_MSmark_proc_norepl: must be a member of the db_owner or sysadmin roles.

sp_MSmark_proc_norepl: invalid object name '%s'.

Could not validate the article '%s'. It is not activated.

Table '%s' may be out of synchronization. Rowcounts (actual: %s, expected: %s). Rowcount method %d used (0 = Full, 1 = Fast).

Table '%s' might be out of synchronization. Rowcounts (actual: %s, expected %s). Checksum values (actual: %s, expected: %s).

Description

20526

20527

20528

20529

20530

20531

20532

20533

20534

20535

20536

20537

20538

20540

20541

20542

20543

20545

20546

20547

20548

20549

20550

20551

20552

20553

20554

20555

20556

20557

Table '%s' passed rowcount (%s) validation. Rowcount method %d used (0 = Full, 1 = Fast).

Table '%s' passed rowcount (%s) and checksum validation. Checksum is not compared for any text or image columns.

Log Reader Agent startup message.

Starting agent.

Run agent.

Detect nonlogged agent shutdown.

Replication agent schedule.

Replication agents checkup

Detects replication agents that are not logging history actively.

Removes replication agent history from the distribution database.

Replication: agent failure

Replication: agent retry

Replication: expired subscription dropped

Replication: agent success

Removes replicated transactions from the distribution database.

Detects and removes expired subscriptions from published databases.

@rowcount_only parameter must be the value 0,1, or 2. 0=7.0 compatible checksum. 1=only check rowcounts. 2=new checksum functionality introduced in version 8.0.

Default agent profile

Verbose history agent profile.

Agent profile for detailed history logging.

Slow link agent profile.

Agent profile for low bandwidth connections.

Windows Synchronization Manager profile

Profile used by the Windows Synchronization Manager.

Could not clean up the distribution transaction tables.

Could not clean up the distribution history tables.

The agent is suspect. No response within last %ld minutes.

6.x publication.

Heartbeats detected for all running replication agents.

Agent shutdown. For more information, see the SQL Server Agent job history for job '%s'.

Description

20558

20559

20560

20561

20562

20563

20564

20565

20566

20567

20568

20569

20570

20571

20572

20573

20574

20575

20576

20577

20578

20579

20580

20581

20582

20583

20584

20585

Table '%s' passed full rowcount validation after failing the fast check. DBCC UPDATEUSAGE will be initiated automatically.

Conditional Fast Rowcount method requested without specifying an expected count. Fast method will be used.

An expected checksum value was passed, but checksums will not be compared because rowcount-only checking was requested.

Generated expected rowcount value of %s for %s.

User delete.

No longer belongs in this partial.

System delete.

Replication: Subscriber has failed data validation

Replication: Subscriber has passed data validation

Agent history clean up: %s

Distribution clean up: %s

Expired subscription clean up

Reinitialize subscriptions having data validation failures

Reinitializes all subscriptions that have data validation failures.

Subscriber '%s' subscription to article '%s' in publication '%s' has been reinitialized after a validation failure.

Replication: Subscription reinitialized after validation failure

Subscriber '%s' subscription to article '%s' in publication '%s' failed data validation.

Subscriber '%s' subscription to article '%s' in publication '%s' passed data validation.

Subscriber '%s' subscription to article '%s' in publication '%s' has been reinitialized after a synchronization failure.

No entries were found in msdb..sysreplicationalerts.

Replication: agent custom shutdown

Generated expected rowcount value of %s and expected checksum value of %s for %s.

Heartbeats not detected for some replication agents. The status of these agents have been changed to 'Failed'.

Cannot drop server '%s' because it is used as a Distributor in replication.

Cannot drop server '%s' because it is used as a Publisher in replication.

Cannot drop server '%s' because it is used as a Subscriber in replication.

Cannot drop server '%s' because it is used as a Subscriber to remote Publisher '%s' in replication.

Validation Failure. Object '%s' does not exist.

Description

20586

20587

20588

20589

20590

20591

20592

20593

20594

20595

20596

20597

20598

20599

(default destination)

Invalid '%s' value for stored procedure '%s'.

The subscription is not initialized. Run the Distribution Agent first.

Agent profile for replicated queued transaction reader.

The article property 'status' cannot include bit 64, 'DTS horizontal partitions' because the publication does not allow data transformations.

Only 'DTS horizontal partitions' and 'no DTS horizontal partitions' are valid 'status' values because the publication allows data transformations.

'dts horizontal partitions' and 'no dts horizontal partitions' are not valid 'status' values because the publication does not allow data transformations.

Cannot modify publication '%s'. The sync_method cannot be changed to 'native', 'concurrent' or 'concurrent_c' because the publication has subscriptions from ODBC or OLE DB Subscribers.

A push subscription to the publication exists. Use sp_subscription_cleanup to drop defunct push subscriptions.

Skipping error signaled.

Only '%s' or members of db_owner can drop the anonymous agent.

Dropped %d anonymous subscription(s).

The row was not found at the Subscriber when applying the replicated command.

Continue on data consistency errors.

Table 3–103:TSQL Error Codes - 20600 to 20699

Description

20600

20601

20602

20603

20604

20605

20606

20607

20608

Agent profile for skipping data consistency errors. It can be used only by SQL
Server Subscribers.

Invalid value specified for agent parameter 'SkipErrors'.

The value specified for agent parameter 'SkipErrors' is too long.

The agent profile cannot be used by heterogeneous Subscribers.

You do not have permissions to run agents for push subscriptions. Make sure that you specify the agent parameter 'SubscriptionType'.

Invalidated the existing snapshot of the publication. Run the Snapshot Agent again to generate a new snapshot.

Reinitialized subscription(s).

Cannot make the change because a snapshot is already generated. Set @force_invalidate_snapshot to 1 to force the change and invalidate the existing snapshot.

Cannot make the change because there are active subscriptions. Set @force_reinit_subscription to 1 to force the change and reinitialize the active subscriptions.

Description

20609

20610

20611

20612

20613

20614

20616

20617

20618

20619

20620

20621

20622

20623

Cannot attach subscription file '%s'. Make sure that it is a valid subscription copy file.

Cannot run '%s' when the Log Reader Agent is replicating the database.

Only table or indexed view to table articles are allowed in publications that allow
DTS.

Checksum validation is not supported because the publication allows DTS. Use row count only validation.

Validation is not supported for articles that are set up for DTS horizontal partitions.

Validation is not supported for heterogeneous Subscribers.

High Volume Server-to-Server Profile

Merge agent profile optimized for the high volume server-to-server synchronization scenario.

You must have CREATE DATABASE permission to attach a subscription database.

Server user '%s' is not a valid user in database '%s'. Add the user account or 'guest' user account into the database first.

The security mode specified requires the server '%s' in sysservers. Use sp_addlinkedserver to add the server.

Cannot copy a subscription database to an existing database.

Replication database option 'sync with backup' cannot be set on the publishing database because the database is in Simple Recovery mode.

You cannot validate article '%s' unless you have 'SELECT ALL' permission on table '%s'.

Table 3–104:TSQL Error Codes - 21000 to 21099

Description

21000

21001

21002

21003

21004

21005

21006

21007

21008

21009

Cannot subscribe to an inactive publication.

Cannot add a Distribution Agent at the Subscriber for a push subscription.

The Distribution Agent for this subscription already exists (%s).

Changing publication names is no longer supported.

Cannot publish the database object '%s' because it is encrypted.

For backward compatibility, sp_addpublisher can be used to add a Publisher for this Distributor. However, sp_adddistpublisher is more flexible.

Cannot use sp_addpublisher to add a Publisher. Use sp_adddistpublisher.

Cannot add the remote Distributor. Make sure that the local server is configured as a Publisher at the Distributor.

Cannot uninstall the Distributor because there are Subscribers defined.

The specified filter procedure is already associated with a table.

Description

21010

21011

21012

21013

21014

21015

21016

21017

21018

21021

21022

21023

21024

21025

21026

21027

21028

21029

21030

21031

21032

21033

21034

21035

Removed %ld replicated transactions consisting of %ld statements in %ld seconds (%ld rows/sec).

Deactivated subscriptions.

Cannot change the 'allow_push' property of the publication to "false". There are push subscriptions on the publication.

Cannot change the 'allow_pull' property of the publication to "false". There are pull subscriptions on the publication.

The @optname parameter value must be 'transactional' or 'merge'.

The replication option '%s' has been set to TRUE already.

The replication option '%s' has been set to FALSE already.

Cannot perform SQL Server 7.0 compatible checksum operation on a merge article that has a vertical or horizontal partition. Rowcount validation and SQL Server 2000 compatible binary checksum operation can be performed on this page.

There are too many consecutive snapshot transactions in the distribution database. Run the Log Reader Agent again or clean up the distribution database.

Drop the Distributor before you uninstall replication.

Cannot set incompatible publication properties. The 'immediate_sync' property of a publication is dependent on the 'independent agent' property of a publication.

'%s' is no longer supported.

The stored procedure '%s' is already published as an incompatible type.

The string being encrypted cannot have null characters.

Cannot have an anonymous subscription on a publication that does not have an independent agent.

'%s' replication stored procedures are not installed. Use sp_replicationoption to install them.

Replication components are not installed on this server. Run SQL Server Setup again and select the option to install replication.

Cannot drop a push subscription entry at the Subscriber unless @drop_push is 'true'.

Names of SQL Server replication agents cannot be changed.

'post_script' is not supported for stored procedure articles.

Could not subscribe because non-SQL Server Subscriber '%s' does not support 'sync tran' update mode.

Cannot drop server '%s' as Distribution Publisher because there are databases enabled for replication on that server.

Rows inserted or updated at the Subscriber cannot be outside the article partition.

You have updated the Publisher property '%s' successfully.

Description

21036

21037

21038

21039

21040

21041

21042

21043

21044

21045

21046

21047

21048

21049

21050

21051

21052

21053

21054

21055

21056

21057

21058

21059

21060

21061

Another %s agent for the subscription(s) is running.

Invalid working directory '%s'.

Windows Authentication is not supported by the server.

The destination owner name is not supported for publications that can have heterogeneous Subscribers. Use native mode bcp for this functionality.

Publication '%s' does not exist.

A remote distribution Publisher is not allowed on this server version.

The distribution Publisher property, 'distributor_password', has no usage and is not supported for a Distributor running on Windows NT 4.0.

The Distributor is not installed.

Cannot ignore the remote Distributor (@ignore_remote_distributor cannot be 1) when enabling the database for publishing or merge publishing.

Cannot uninstall the Distributor because there are databases enabled for publishing or merge publishing.

Cannot change distribution Publisher property 'distribution_db' because the remote Publisher is using the current distribution database.

Cannot drop the local distribution Publisher because there are Subscribers defined.

Cannot add login '%s' to the publication access list because it does not have access to the distribution server '%s'.

The login '%s' does not have access permission on publication '%s' because it is not in the publication access list.

Only members of the sysadmin or db_owner roles can perform this operation.

Could not subscribe because non-SQL Server Subscriber '%s' does not support custom stored procedures.

Queued Updating Subscriptions: write to message queue failed.

The parameter must be one of the following: 'description', 'status', 'retention', 'sync_mode', 'allow_push', 'allow_pull', 'allow_anonymous', 'enabled_for_internet', 'centralized_conflicts', 'conflict_retention', or 'snapshot_ready'.

Updatable Subscribers: RPC to Publisher failed.

Invalid parameter %s specified for %s.

The subscription to publication '%s' has expired and does not exist.

Anonymous Subscribers cannot have updatable subscriptions.

An updatable subscription to publication '%s' on Subscriber '%s' already exists.

Cannot reinitialize subscriptions of non-immediate_sync publications.

Could not subscribe because non-SQL Server Subscriber '%s' does not support parameterized statements.

Invalid article status %d specified when adding article '%s'.

Description

21062

21063

21064

21070

21071

21072

21073

21074

21075

21076

21077

21078

21079

21080

21081

21082

21083

21084

21085

21086

21087

21088

The row size of table '%s' exceeds the replication limit of 6,000 bytes.

Table '%s' cannot participate in updatable subscriptions because it is published for merge replication.

The subscription is uninitialized or unavailable for immediate updating as it is marked for reinitialization. If using queued failover option, run Queue Reader Agent for subscription initialization. Try again after the (re)initialization completes.

This subscription does not support automatic reinitialization (subscribed with the 'no sync' option). To reinitialize this subscription, you must drop and re-create the subscription.

Cannot reinitialize article '%s' in subscription '%s:%s' to publication '%s' (subscribed with the 'no sync' option).

The subscription has not been synchronized within the maximum retention period or it has been dropped at the Publisher. You must reinitialize the subscription to receive data.

The publication specified does not exist.

The subscription has been marked inactive and must be reinitialized at the Publisher. Contact the database administrator.

The initial snapshot for publication '%s' is not yet available.

The initial snapshot for article '%s' is not yet available.

Deactivated initial snapshot for anonymous publication(s). New subscriptions must wait for the next scheduled snapshot.

Table '%s' does not exist in the Subscriber database.

The RPC security information for the Publisher is missing or invalid. Use sp_link_publication to specify it.

The 'msrepl_tran_version' column must be in the vertical partition of the article
that is enabled for updatable subscriptions; it cannot be dropped.

Server setting 'Allow triggers to be fired which fire other triggers (nested triggers)' must exist on updatable Subscribers.

Database property 'IsRecursiveTriggersEnabled' has to be false for subscription databases at Subscribers that allow updatable subscriptions.

Database compatibility level at immediate updating Subscribers cannot be less than 70.

Publication '%s' does not allow anonymous subscriptions.

The retention period must be less than the retention period for the distribution database.

The retention period for the distribution database must be greater than the retention period of any existing non-merge publications.

Anonymous Subscribers or Subscribers at this server are not allowed to create merge publications.

The initial snapshot for the publication is not yet available.

Table 3–105:TSQL Error Codes - 21100 to 21199

Description

21107

21108

21109

21110

21111

21112

21113

21114

21115

21116

21117

21118

21119

21120

21121

21122

21123

21124

21125

21126

21127

21128

21129

'%ls' is not a table or view.

This edition of SQL Server does not support transactional publications.

The parameters @xact_seqno_start and @xact_seqno_end must be identical if @command_id is specified.

@xact_seqno_start and @publisher_database_id must be specified if @command_id is specified.

'%s' is not a valid parameter for the Snapshot Agent.

'%s' is not a valid parameter for the Log Reader Agent.

'%s' is not a valid parameter for the Distribution Agent.

'%s' is not a valid parameter for the Merge Agent.

'%s' is not a valid value for the '%s' parameter. The value must be a positive integer.

'%s' is not a valid value for the '%s' parameter. The value must be 1, 2, or 3.

'%s' is not a valid value for the '%s' parameter. The value must be 0, 1, or 2.

'%s' is not a valid value for the '%s' parameter. The value must be greater than or equal to 0 and less than or equal to 10,000.

'%s' is not a valid value for the '%s' parameter. The value must be a non-negative integer.

Only members of the sysadmin fixed server role and db_owner fixed database role can drop subscription '%s' to publication '%s'.

Only members of the sysadmin fixed server role and '%s' can drop the pull subscription to the publication '%s'.

Cannot drop the distribution database '%s' because it is currently in use.

The agent profile '%s' could not be found at the Distributor.

Cannot find the table name or the table owner corresponding to the alternative table ID(nickname) '%d' in sysmergearticles.

A table used in merge replication must have at least one non-computed column.

Pull subscriptions cannot be created in the same database as the publication.

Only global merge subscriptions can be added to database '%s'.

Terminating immediate updating or queued updating INSERT trigger because it is not the first trigger to fire. Use sp_settriggerorder procedure to set the firing order for trigger '%s' to first.

Terminating immediate updating or queued updating UPDATE trigger because it is not the first trigger to fire. Use sp_settriggerorder procedure to set the firing order for trigger '%s' to first.

Description

21130

21131

21132

21133

21134

21135

21136

21137

21138

21139

21140

21141

21142

21143

21144

21145

21146

21147

21148

21149

21150

21151

Terminating immediate updating or queued updating DELETE trigger because it is not the first trigger to fire. Use sp_settriggerorder procedure to set the firing order for trigger '%s' to first.

There are existing subscriptions to heterogeneous publication '%s'. To add new articles, first drop the existing subscriptions to the publication.

Cannot create transactional subscription to merge publication '%s'.The publication type should be either transactional(0) or snapshot(1) for this operation.

Publication '%s' is not enabled to use an independent agent.

The specified job ID must identify a Distribution Agent or a Merge Agent job.

Detected inconsistencies in the replication agent table. The specified job ID does not correspond to an entry in '%ls'.

Detected inconsistencies in the replication agent table. The specified job ID corresponds to multiple entries in '%ls'.

This procedure supports only remote execution of push subscription agents.

The 'offload_server' property cannot be the same as the Distributor name.

Could not determine the Subscriber name for distributed agent execution.

Agent execution cannot be distributed to a Subscriber that resides on the same server as the Distributor.

The @change_active flag may not be specified for articles with manual filters or views.

The SQL Server '%s' could not obtain Windows group membership information for login '%s'. Verify that the Windows account has access to the domain of the login.

The custom stored procedure schema option is invalid for a snapshot publication article.

Cannot subscribe to publication of sync_type 'dump database' because the Subscriber has subscriptions to other publications.

Cannot subscribe to publication %s because the Subscriber has a subscription to a publication of sync_type 'dump database'.

@use_ftp cannot be 'true' while @alt_snapshot_folder is neither NULL nor empty.

The '%s' database is not published for merge replication.

Both @subscriber and @subscriberdb must be specified with non-null values simultaneously, or both must be left unspecified.

The '%s' database is not published for transactional or snapshot replication.

Unable to determine the snapshot folder for the specified subscription because the specified Subscriber is not known to the Distributor.

Pre- and post-snapshot commands are not supported for a publication that may support non-SQL Server Subscribers by using the character-mode bcp as the synchronization method.

Description

21152

21153

21154

21156

21157

21158

21159

21160

21161

21162

21163

21164

21165

21166

21167

21168

21169

21170

21171

21172

21173

21174

21175

Cannot create a subscription of sync_type 'none' to a publication using the 'concurrent' or 'concurrent_c' synchronization method.

Cannot create article '%s'. All articles that are part of a concurrent synchronization publication must use stored procedures to apply changes to the Subscriber.

Cannot change article '%s'. All articles that are part of a concurrent synchronization publication must use stored procedures to apply changes to the Subscriber.

The @status parameter value must be 'initiated' or 'active'.

The snapshot compression option can be enabled only for a publication having an alternate snapshot generation folder defined.

For a publication to be enabled for the Internet, the 'ftp_address' property must not be null.

If a publication is enabled for the Internet, the 'alt_snapshot_folder' property must be non-empty.

The @ftp_port parameter cannot be NULL.

Could not change the Publisher because the subscription has been dropped. Use sp_subscription_cleanup to clean up the triggers.

It is invalid to exclude the rowguid column for the table from the partition.

It is not possible to add column '%s' to article '%s' because the snapshot for publication '%s' has been run.

Column '%s' cannot be included in a vertical partition because it is neither nullable nor defined with a default value.

Column '%s' cannot be excluded from a vertical partition because it is neither nullable nor defined with a default value.

Column '%s' does not exist.

The specified job ID does not represent a %s agent job for any push subscription in this database.

Only members of the sysadmin fixed server role, members of the db_owner fixed database role, and owners of subscriptions served by the specified replication agent job can modify the agent offload settings.

Could not identify the Publisher '%s' at the Distributor '%s'. Make sure that '%s' is registered in the sysservers table at the Distributor.

Only a SQL Server 2000 or OLE DB Subscriber can use DTS.

Could not find package '%s' in msdb at server '%s'.

The publication has to be in 'character' or 'concurrent_c' bcp mode to allow DTS.

The publication has to be 'independent_agent type' to allow DTS.

You must use default values for @ins_cmd, @upd_cmd, and @del_cmd, and @status can be only 16 or 80 because the publication allows DTS.

You cannot change 'ins_cmd','upd_cmd', or 'del_cmd' article properties because the publication allows DTS or queued updating option.

Description

21176

21177

21178

21179

21180

21181

21182

21183

21184

21185

21186

21187

21188

21189

21190

21191

21192

21193

21194

21195

21196

21197

21198

21199

Only members of the sysadmin fixed server role, db_owner fixed database role, or the creator of the subscription can change the subscription properties.

Could not create column list because it is too long. Create the list manually.

DTS properties cannot be set because the publication does not allow for data transformation.

Invalid @dts_package_location parameter value. Valid options are 'Distributor' or 'Subscriber'.

A publication that allows DTS cannot be enabled for updatable subscriptions.

@dts_package_name can be set for push subscriptions only.

The @agent_type parameter must be one of 'distribution', 'merge', or NULL.

Invalid property name '%s'.

%s parameter is incorrect: it should be '%s', '%s' or '%s'.

The subscription is not initialized or not created for failover mode operations.

Subscription for Publisher '%s' does not have a valid queue_id.

The current mode is the same as the requested mode.

Changed update mode from [%s] to [%s].

The queue for this subscription with queue_id = '%s' is not empty. Run the Queue Reader Agent to make sure the queue is empty before setting mode from [queued] to [immediate].

Overriding queue check for setting mode from [%s] to [%s].

Values for @ins_cmd, @upd_cmd, and @del_cmd can be only [%s], [%s] and [%s] respectively because the publication allows queued transactions.

MSrepl_tran_version column is a predefined column used for replication and can be only of data type uniqueidentifier

@identity_range, @pub_identity_range, or @threshold cannot be NULL when @auto_identity_support is set to TRUE.

Cannot support identity_range_control because this table does not have an identity column.

A valid identity range is not available. Check the data type of the identity column.

Identity automation failed.

Failed to allocate new identity range.

Schema replication failed.

This change cannot take effect until you run the snapshot again.

Table 3–106:TSQL Error Codes - 21200 to 21299

Description

21200

Publication '%s' does not exist.

Description

21201

21202

21203

21204

21205

21206

21207

21208

21209

21210

21211

21212

21213

21214

21215

21216

21217

21218

21219

21220

21221

21222

21223

21224

Dropping a column that is being used by a merge filter clause is not allowed.

It is not possible to drop column '%s' to article '%s' because the snapshot for publication '%s' has already been run.

Duplicate rows found in %s. Unique index not created.

The publication '%s' does not allow subscription copy or its subscription has not been synchronized.

The subscription cannot be attached because the publication does not allow subscription copies to synchronize changes.

Cannot resolve load hint for object %d because the object is not a user table.

Cannot find source object ID information for article %d.

This step failed because column '%s' exists in the vertical partition.

This step failed because column '%s' does not exist in the vertical partition.

The publication must be immediate_sync type to allow subscription copy.

The database is attached from a subscription copy file without using sp_attach_subscription. Drop the database and reattach it using sp_attach_subscription.

Cannot copy subscription. Only single file subscription databases are supported for this operation.

Non-SQL Server Subscribers cannot subscribe to publications that allow DTS without using a DTS package.

Cannot create file '%s' because it already exists.

An alternate synchronization partner can be configured only at the Publisher.

Publisher '%s', publisher database '%s', and publication '%s' are not valid synchronization partners.

Publication of '%s' data from Publisher '%s'.

The creation_script property cannot be NULL if a schema option of 0x0000000000000000 is specified for the article.

The specified source object must be a stored procedure object if it is published as a 'proc schema only' type article.

Unable to add the article '%s' because a snapshot has been generated for the publication '%s'.

The specified source object must be a view object if it is going to be as a 'view schema only' type article.

The @schema_option parameter for a procedure or function schema article can include only the options 0x0000000000000001 or 0x0000000000002000.

The @pre_creation_command parameter for a schema only article must be either 'none' or 'drop'.

'%s' is not a valid property for a schema only article.

Description

21225

21226

21227

21228

21229

21230

21231

21232

21233

21234

21235

21236

21237

21238

21239

21240

21241

21242

21243

21244

21245

21246

21247

The 'offload_server' property cannot be NULL or empty if the pull subscription agent is to be enabled for remote activation.

The database '%s' does not have a pull subscription to the specified publication.

The 'offload_server' property cannot be the same as the Subscriber server name.

The specified source object must be a user-defined function object if it is going to be published as a 'func schema only' type article.

The only schema options available for a view schema article are:
0x0000000000000001, 0x0000000000000010, 0x0000000000000040, 0x0000000000000100, and 0x0000000000002000.

Do not call this stored procedure for schema change because the current database is not enabled for replication.

Automatic identity range support is useful only for publications that allow queued updating.

Identity range values must be positive numbers that are greater than 1.

Threshold value must be from 1 through 100.

Cannot use the INSERT command because the table has an identity column. The insert custom stored procedure must be used to set 'identity_insert' settings at the Subscriber.

Article property '%s' can be set only when the article uses automatic identity range management.

The subscription(s) to Publisher '%s' does not allow subscription copy or it has not been synchronized.

There is a push subscription to Publisher '%s'. Only pull and anonymous subscriptions can be copied.

There is a push subscription to publication '%s'. Only pull and anonymous subscriptions can be copied.

Cannot copy subscriptions because there is no synchronized subscription found in the database.

The table '%s' is already published as another article with a different automatic identity support option.

The threshold value should be from 0 through 99.

Conflict table for article '%s' could not be created successfully.

Publisher '%s', publication database '%s', and publication '%s' could not be added to the list of synchronization partners.

Character mode publication does not support vertical filtering when the base table does not support column-level tracking.

Table '%s' is not part of publication '%s'.

This step failed because table '%s' is not part of any publication.

Cannot create file at '%s'. Ensure the file path is valid.

Description

21248

21249

21250

21251

21252

21253

21254

21255

21256

21257

21258

21259

21260

21261

21262

21263

21264

21265

21266

21267

21268

21269

Cannot attach subscription file '%s'. Ensure the file path is valid and the file is updatable.

OLE DB or ODBC Subscribers cannot subscribe to article '%s' in publication '%s' because the article has a timestamp column and the publication is 'allow_queued_tran' (allows queued updating subscriptions).

Primary key column '%s' cannot be excluded from a vertical partition.

Publisher '%s', publisher database '%s', publication '%s' could not be removed from the list of synchronization partners.

It is invalid to remove the default Publisher '%s', publication database '%s', and publication '%s' from the list of synchronization partners

Parameter '@add_to_active_directory' cannot be set to TRUE because Active Directory client package is not installed properly on the machine where SQL Server is running.

The Active Directory operation on publication '%s' could not be completed bacause Active Directory client package is not installed properly on the machine where SQL Server is running.

Column '%s' already exists in table '%s'.

A column used in filter clause '%s' either does not exist in the table '%s' or cannot be excluded from the current partition.

Invalid property '%s' for article '%s'.

You must first drop all existing merge publications to add an anonymous or local subscription to database '%s'.

Invalid property value '%s'.

Schema replication failed because database '%s' on server '%s' is not the original Publisher of table '%s'.

The offload server must be specified if the agent for this subscription is to be offloaded for remote execution.

Failed to drop column '%s' from the partition because a computed column is accessing it.

Parameter '%s' cannot be NULL or an empty string.

Column '%s' cannot be dropped from table '%s' because it is a primary key column.

Column '%s' cannot be dropped from table '%s' because there is a unique index accessing this column.

Cannot publish table '%s' for both a merge publication and a publication with the queued updating option .

Invalid value for queue type was specified. Valid values = (%s).

Cannot change queue type while there are subscriptions to the publication.

Cannot add a computed column or a timestamp column to a vertical partition for a character mode publication.

Description

21270

21272

21273

21274

21275

21276

21277

21278

21279

21280

21281

21282

21283

21284

21285

21286

21287

21288

21289

21290

21291

21292

21293

Queued snapshot publication property '%s' cannot have the value '%s'.

Cannot clean up the meta data for publication '%s' because other publications are using one or more articles in this publication.

You must upgrade the Subscriber to SQL Server 2000 to create updatable subscriptions to SQL Server 2000 Publishers.

Invalid publication name '%s'.

The schema-bound view '%ls' can be published only as 'indexed view schema only' or a log-based indexed view (transactional only) article.

The type must be 'table' or '( view | indexed view | proc | func ) schema only'.

The source object '%ls' must be a schema-bound view to be published as 'indexed view schema only' or a log-based indexed view article.

The source object '%ls' must be a schema-bound view with at least a clustered index to be published as a log-based indexed view article.

The 'schema_option' property for a merge article cannot be changed after a snapshot is generated for the publication. To change the 'schema_option' property of this page the corresponding merge publication must be dropped and re-created.

Publication '%s' cannot be subscribed to by Subscriber database '%s' because it contains one or more articles that have been subscribed to by the same Subscriber database at transaction level.

Publication '%s' cannot be subscribed to by Subscriber database '%s' because it contains one or more articles that have been subscribed to by the same Subscriber database at merge level.

@identity_range, @pub_identity_range, and @threshold must be NULL when @auto_identity_support is set to FALSE.

Column '%s' of table '%s' cannot be excluded from a vertical partition because there is a computed column that depends on it.

Failed to drop column '%s' from table '%s'.

Failed to add column '%s' to table '%s'.

Conflict table '%s' does not exist.

The specified @destination_folder is not a valid path of an existing folder.

Could not create the snapshot directory structure in the specified @destination_folder.

Either the snapshot files have not been generated or they have been cleaned up.

Identity range value is too large for the data type of the identity column.

The specified automatic identity support parameters conflict with the settings in another article.

Object '%s' cannot be published twice in the same publication.

Warning: adding updatable subscription for article '%s' may cause data inconsistency as the source table is already subscribed to '%s'

Description

21294

21295

21296

21297

21298

21299

Either @publisher (and @publisher_db) or @subscriber (and @subscriber_db) must be specified, but both cannot be specified.

Publication '%s' does not contain any article that uses automatic identity range management.

Parameter @resync_type must be either 0, 1, 2.

Invalid resync type. No validation has been performed for this subscription.

Failed to resynchronize this subscription.

Invalid Subscriber partition validation expression '%s'.

Table 3–107:TSQL Error Codes - 21300 to 21399

Description

21300

21301

21302

21303

21304

21305

21306

21307

21308

21309

21310

21311

21312

21313

21314

21315

21316

21317

21318

The resolver information was specified without specifying the resolver to be used for article '%s'. The default resolver will be used.

The resolver information should be specified while using the '%s' resolver.

The resolver information should specify a column with data type, datetime, or smalldatetime while using the '%s' resolver.

The article '%s' should enable column tracking to use the '%s' resolver. The default resolver will be used to resolve conflicts on this page.

The merge triggers could not be created on the table '%s'.

The schema change information could not be updated at the subscription database.

The copy of the subscription could not be made because the subscription to publication '%s' has expired.

The subscription could not be attached because the subscription to publication '%s' has expired.

Rowcount validation profile.

Profile used by the Merge Agent to perform rowcount validation.

Rowcount and checksum validation profile.

Profile used by the Merge Agent to perform rowcount and checksum validation.

Cannot change this publication property because there are active subscriptions to this publication.

Subscriber partition validation expression must be NULL for static publications.

There must be one and only one of '%s' and '%s' that is not NULL.

Failed to adjust Publisher identity range for table '%s'.

Failed to adjust Publisher identity range for publication '%s'.

A push subscription to the publication '%s' already exists. Use sp_mergesubscription_cleanup to drop defunct push subscriptions.

Table '%s' must have at least one column that is included in the vertical partition.

Description

21319

21320

21321

21323

21324

21325

21326

21327

21328

21329

21330

21331

21332

21333

21334

21335

21336

21337

21338

21339

21340

21341

21342

21343

21344

21345

21346

Could not find the Snapshot Agent command line for the specified publication.

This version of the Publisher cannot use a SQL Server 7.0 Distributor.

The parameter @dynamic_snapshot_location cannot be an empty string.

A dynamic snapshot job can be scheduled only for a publication with dynamic filtering enabled.

A Snapshot Agent must be added for the specified publication before a dynamic snapshot job can be scheduled.

Could not find the Snapshot Agent ID for the specified publication.

Could not find the dynamic snapshot job with a '%ls' of '%ls' for the specified publication.

'%ls' is not a valid dynamic snapshot job name.

The specified dynamic snapshot job name '%ls' is already in use. Try the operation again with a different job name.

Only one of the parameters, @dynamic_snapshot_jobid or @dynamic_snapshot_jobname, can be specified with a nondefault value.

Failed to create a sub-directory under the replication working directory.(%ls)

Failed to copy user script file to the Distributor.(%ls)

Failed to retrieve information about the publication : %ls. Check the name again.

Protocol error. Message indicates a generation has disappeared.

Cannot initialize Message Queuing-based subscription because the platform is not Message Queuing %s compliant

Warning: column '%s' already exists in the vertical partition already.

Warning: column '%s' does not exist in the vertical partition.

Invalid @subscriber_type value. Valid options are 'local' and 'global'.

Cannot drop article '%s' from publication '%s' because its snapshot has been run and this publication could have active subscriptions.

Warning: the publication uses a feature that is only supported only by Ssubscribers running '%s' or higher.

On Demand user script cannot be applied to the snapshot publication.

@dynamic_snapshot_location cannot be a non-empty string while @alt_snapshot_folder is neither empty nor null.

@dynamic_snapshot_location cannot be a non-empty string while @use_ftp is 'true'.

Could not find stored procedure '%s'.

Invalid value specified for %ls parameter.

Excluding the last column in the partition is not allowed.

Failed to change the owner of '%s' to '%s'.

Description

21347

21348

21349

21350

21351

21352

21353

21354

21355

21356

21357

21358

21359

21360

21361

21362

21363

21364

21365

Column '%s' cannot be excluded from the vertical partitioning because there is a unique index accessing this column.

Invalid property name '%s'.

Warning: only Subscribers running SQL Server 7.0 Service Pack 2 or later can synchronize with publication '%s' because decentralized conflict logging is designated.

Warning: only Subscribers running SQL Server 2000 can synchronize with publication '%s' because a compressed snapshot is used.

Warning: only Subscribers running SQL Server 2000 can synchronize with publication '%s' because vertical filters are being used.

Warning: only Subscribers running SQL Server 2000 can synchronize with publication '%s' because schema replication is performed.

Warning: only Subscribers running SQL Server 7.0 Service Pack 2 or later can synchronize with publication '%s' because publication wide reinitialization is performed.

Warning: only Subscribers running SQL Server 2000 can synchronize with publication '%s' because publication wide reinitialization is performed.

Warning: only Subscribers running SQL Server 7.0 Service Pack 2 or later can synchronize with publication '%s' because merge metadata cleanup task is performed.

Warning: only Subscribers running SQL Server 7.0 Service Pack 2 or later can synchronize with publication '%s' because publication wide validation task is performed.

Warning: only Subscribers running SQL Server 2000 can synchronize with publication '%s' because data types new in SQL Server 2000 exist in one of its articles.

Warning: only Subscribers running SQL Server 2000 can synchronize with publication '%s' because at least one timestamp column exists in one of its articles..

Warning: only Subscribers running SQL Server 2000 can synchronize with publication '%s' because automatic identity ranges are being used.

Warning: only Subscribers running SQL Server 2000 can synchronize with publication '%s' because a new article has been added to the publication after its snapshot has been generated.

The specified @agent_jobid is not a valid job id for a '%s' agent job.

Merge filter '%s' does not exist.

Failed to add publication '%s' to Active Directory. %s

Could not add article '%s' because a snapshot is already generated. Set @force_invalidate_snapshot to 1 to force this and invalidate the existing snapshot.

Could not add article '%s' because there are active subscriptions. Set @force_reinit_subscription to 1 to force this and reintialize the active subscriptions.

Description

21366

21367

21368

21369

21370

21371

21372

21373

21374

21375

21376

21377

21378

21379

21380

21381

21382

21383

21384

21385

21386

Could not add filter '%s' because a snapshot is already generated. Set @force_invalidate_snapshot to 1 to force this and invalidate the existing snapshot.

Could not add filter '%s' because there are active subscriptions. Set @force_reinit_subscription to 1 to force this and reintialize the active subscriptions.

The specified offload server name contains the invalid character '%s'.

Could not remove publication '%s' from Active Directory.

The resync date specified '%s' is not a valid date.

Could not propagate the change on publication '%s' to Active Directory.

Cannot drop filter '%s' from publication '%s' because its snapshot has been run and this publication could have active subscriptions.

Could not open database %s. Replication settings and system objects could not be upgraded. If the database is used for replication, run sp_vupgrade_replication in the [master] database when the database is available.

Upgrading distribution settings and system objects in database %s.

Upgrading publication settings and system objects in database %s.

Could not open database %s. Replication settings and system objects could not be upgraded. If the database is used for replication, run sp_vupgrade_replication in the [master] database when the database is available.

Upgrading subscription settings and system objects in database %s.

Could not open distribution database %s because it is offline or being recovered. Replication settings and system objects could not be upgraded. Be sure this database is available and run sp_vupgrade_replication again.

Cannot drop article '%s' from publication '%s' because a snapshot is already generated. Set @force_invalidate_snapshot to 1 to force this and invalidate the existing snapshot.

Cannot add identity column without forcing reinitialization. Set @force_reinit_subscription to 1 to force reinitialization.

Cannot add (drop) column to table '%s' because the table belongs to publication(s) with an active updatable subscription. Set @force_reinit_subscription to 1 to force reinitialization.

Cannot drop filter '%s' because a snapshot is already generated. Set @force_invalidate_snapshot to 1 to force this and invalidate the existing snapshot.

Cannot enable a merge publication on this server because the working directory of its Distributors is not using a UNC path.

The specified subscription does not exist or has not been synchronized yet.

Snapshot failed to process publication '%s'. Possibly due to active schema change activity.

Schema change failed on publication '%s'. Possibly due to active snapshot or other schema change activity.

Description

21387

21388

21389

21390

21391

21392

21393

21394

21395

21396

The expanded dynamic snapshot view definition of one of the articles exceeds the system limit of 3499 characters. Consider using the default mechanism instead of the dynamic snapshot for initializing the specified subscription.

The concurrent snapshot for publication '%s' has not been activated by the Log
Reader Agent.

Warning: only Subscribers running SQL Server 2000 can synchronize with publication '%s' because column-level collation is scripted out with the article schema creation script.

Warning: only Subscribers running SQL Server 2000 can synchronize with publication '%s' because extended properties are scripted out with the article schema creation script.

Warning: only Subscribers running SQL Server 2000 can synchronize with publication '%s' because it contains schema-only articles.

Row filter(%s) is invalid for column partition(%s) for article '%s' in publication '%s'.

Dropping row filter(%s) for article '%s' in '%s'. Reissue sp_articlefilter and sp_articleview to create a row filter.

Invalid schema option specified for Queued updating publication. Need to set the schema option to include DRI constraints.

This column cannot be included in a transactional publication because the column ID is greater than 255.

The subscription is marked inactive and must be dropped and re-created.

Table 3–108:TSQL Error Codes - 21400 to 21499

Description

21400

21401

21402

21403

21404

21405

21406

21413

21414

21415

21416

21417

Article property must be changed at the original Publisher of article '%s'.

Article name cannot be 'all'.

Incorrect value for parameter '%s'.

The 'max_concurrent_dynamic_snapshots' publication property must be greater than or equal to zero.

'%s' is not a valid value for the '%s' parameter. The value must be a positive integer greater than 300 or 0.

'%s' is not a valid value for the '%s' parameter. The value must be an integer greater than or equal to %d.

'%s' is not a valid value for the '%s' parameter. The value must be 0 or 1.

Failed to acquire the application lock indicating the front of the queue.

Unexpected failure acquiring application lock.

Unexpected failure releasing application lock.

Property '%s' of article '%s' cannot be changed.

Having a queue timeout value of over 12 hours is not allowed.

Description

21418

21419

21420

21421

Failed to add column '%s' to table '%s' because of metadata overflow.

Filter '%s' of article '%s' cannot be changed.

Subscription property '%s' cannot be changed.

Article '%s' cannot be dropped because there are other articles using it as a join article.

Table 3–109:TSQL Error Codes - 21500 to 21599

Description

21500

21501

21502

21503

21504

21505

21506

21507

21508

21509

21510

21511

Invalid subscription type is specified. A subscription to publication '%s' already exists in the database with a different subscription type.

The supplied resolver information does not specify a valid column name to be used for conflict resolution by '%s'.

The publication '%s' does not allow the subscription to synchronize to an alternate synchronization partner.

Cleanup of merge meta data cannot be performed while merge processes are running. Retry this operation after the merge processes have completed.

Cleanup of merge meta data at republisher '%s'.'%s' could not be performed because merge processes are propagating changes to the republisher. All subscriptions to this republisher must be reinitialized.

Changes to publication '%s' cannot be merged because it has been marked inactive.

sp_mergecompletecleanup cannot be executed before sp_mergepreparecleanup is executed. Use sp_mergepreparecleanup to initiate the first phase of merge meta data cleanup.

All prerequisites for cleaning up merge meta data have been completed. Execute sp_mergecompletecleanup to initiate the final phase of merge meta data cleanup.

Cleanup of merge meta data cannot be performed while merge processes are running. Cleanup will proceed after the merge processes have completed.

Cleanup of merge meta data cannot be performed because some republishers have not quiesced their changes. Cleanup will proceed after all republishers have quiesced their changes.

Data changes are not allowed while cleanup of merge meta data is in progress.

Neither MSmerge_contents nor MSmerge_tombstone contain meta data for this row.

### 4.1 General System Error Messages

The following table lists the InterSystems IRIS® system error messages. If a system process terminates with an error, it reports the error message via the operator console facility.

Table 4–1: System Error Messages

<ALARM>

Description

An internal timer for user events has expired.

<ARRAY DIMENSION>

The expected dimensionality of the variable or argument is incorrect.

<BAD IMPLICIT>

Invalid implicit data conversion requested.

<BLOCKNUMBER>

A reference has been made to a block outside the range of the database file.

<_CALLBACK SYNTAX>

(Note underscore in error code name.) A name has been specified beginning with an underscore character followed by a letter.

<CANNOT GET THIS
PROPERTY>

<CANNOT SET THIS
PROPERTY>

<CLASS COMPILING>

There has been an attempt to get a property of a class for which getting this property is invalid.

There has been an attempt to set a property of a class for which setting this property is invalid.

There has been an attempt to instantiate a class or invoke a class method of a class which is currently being recompiled on the local system.

<CLASS DESCRIPTOR>

There has been an attempt to run a routine which is actually a class descriptor.

<CLASS DOES NOT EXIST>

A reference has been made to a nonexistent class. For further details, refer
to $ZERROR.

<CLASS EDITED>

There has been an attempt to use an object hosted on the local system whose class has been recompiled from a remote system since the object was created.

<CLASS PROPERTY>

InterSystems IRIS does not support class properties. Class property syntax generates a compile error. Attempts to issue a class property reference by calling a propertyGet() instance method as a class method fails with this error. Rewrite as a proper class method instead of a calculated property.

Description

<CLASS RECOMPILED>

There has been an attempt to use an object hosted on the local system whose class has been recompiled on the local system since the object was created.

<CLASS TOO BIG TO
LOAD>

<CLASS TOO BIG TO
SAVE>

<CLIENT-SERVER
MISMATCH>

A class cannot be used because its class descriptor is too large to fit into a routine buffer.

A class cannot be created because its class descriptor is too large to fit into a routine buffer.

A network request cannot be processed due to incompatibility between the client and server.

<CLUSTERFAIL>

A cluster member has failed during global buffer lock processing.

<COLLATECHANGE>

There was an attempt to change the collation algorithm while subscripted local variables are defined.

<COLLATEMISMATCH>

Subscript level mapping failed due to misconfigured collation type.

<COLLATION NOT
SUPPORTED>

<COMMAND>

<COMMITFAIL>

A reference has been made to a global whose collation type is not supported on the current system.

A command has been used improperly in this context, such as an
argumentless GoTo in a routine. For further details, refer to $ZERROR.

Received during a COMMIT when InterSystems IRIS receives an error while processing a TCommit. This error means that InterSystems IRIS is not sure whether one or more remote machines actually processed the commit.

<COMPLEX PATTERN>

The combination of pattern and input string generate too many possible matches to manage.

<CONFLICTING BLOCK
NUMBERS>

<CORRUPT OBJECT>

There has been an attempt to reserve a block that was already reserved.

An internal object system error occurred. Contact InterSystems Worldwide Response Center if this error occurs.

<CORRUPT VOLUME SET>

The volume set is corrupted. Usually, this means the label on the volume set is wrong. Use the LABEL utility to correct it.

<CP NOT STARTED>

One of the major processes required for proper operation of the system failed
to start. This is potentially a very serious system error; notify your system
manager.

<DATABASE MAP LABEL>

There is an invalid label in a database map block.

<DATABASE>

<DIRECTORY>

<DISCONNECT>

<DISKHARD>

InterSystems IRIS has detected degradation in this database (this is potentially
a very serious system error; notify your system manager).

There is no such directory on the target system, no InterSystems IRIS database, the InterSystems IRIS database is not mounted, or the database
is locked by another configuration. For further details, refer to $ZERROR.

A TCP disconnect has been detected while a long-duration request is being processed.

InterSystems IRIS has encountered an uncorrectable disk hardware error
(this may also be the result of a database problem; notify your system
manager).

<DIVIDE>

Description

There has been an attempt to divide by zero.

<DOMAINSPACERETRY>

Repeated attempts to contact the domain space master have failed.

<DSCON>

<DSKFUL>

There has been an attempt to read from a disconnected terminal.

An attempt to write data to a disk file failed because the file reached its
maximum size; some of the data was written but not all.

<DUPLICATEARG>

There has been an attempt use $SORTBEGIN with an ancestor or descendent
of an already-defined $SORTBEGIN global.

<DYNAMIC LIBRARY LOAD>

An error has occurred during an attempt to load a dynamic library via callout. See messages.log for additional information.

<ECODETRAP>

<EDITED>

<ENDOFFILE>

<ERRTRAP>

A user-generated software trap was generated by setting the $ECODE system
variable to a non-null string value.

Incorrect modification of a routine has resulted in a mismatch, such as two copies of a routine with the same name but different timestamps, or the class routine does not match the class descriptor. For example, compiling a routine, then using ZLOAD and ZSAVE on the routine would result in a timestamp mismatch. Also, if the connection to the data server suffers a network outage (neither application server nor data server shuts down), the routines downloaded from the data server are marked as if they had been edited.

There has been an attempt to read past the end-of-file marker of a sequential file.

There are insufficient system resources remaining to run an error trap procedure.

<EXTERNAL INTERRUPT>

Another process has attempted to interrupt this process.

<FILEFULL>

InterSystems IRIS attempted to allocate a disk block for more global data or routine storage, but the attempt failed because the InterSystems IRIS database is full and could not be expanded.

<FRAMESTACK>

The routine has too many nested calls to Do, For, Xecute, New, or
user-written functions. For further details, refer to $ZERROR.

<FUNCTION>

The specified function does not exist or is being used improperly.

<GARBAGE COLLECTOR
FAILED>

One of the processes that reclaims space in the database has failed. This is
potentially a very serious system error; notify your system manager.

<HALTED>

An internal error message.

<ILLEGAL VALUE>

There has been an attempt to use a negative value where one is not allowed,
such as, for $X or $Y.

<INSUFFICIENT CLASS
MEMORY>

A class cannot be used because InterSystems IRIS has run out of shared memory.

<INTERNAL OBJECT
ERROR>

An internal object system error. Contact InterSystems Worldwide Response Center if this error occurs.

<INTERRUPT>

A user has interrupted the routine. (In many implementations, the user has pressed CTRL-C.)

Description

<INVALID ARGUMENT>

There is an invalid argument prototype in the zfentry specification of a callout function.

<INVALID BIT STRING>

The bit string used in a bit string operation is not valid.

<INVALID CLASS>

There has been an attempt to use a class that has been corrupted. Recompile the class and try again.

<INVALID FILE VARIABLE>

A file variable was expected but none was supplied.

<INVALID GLOBAL
REFERENCE>

A global reference failed length validation.

<INVALID OREF>

No object with the specified OREF is currently in memory.

<INVALID SELECT LIST>

A SELECT list was expected but not supplied.

<INVALID TYPE>

An OREF has been used where not allowed.

<Java Exception>

An exception occurred during a call into the Java runtime environment.

<Java VM not loaded>

No Java Virtual Machine is available.

<LABELREDEF>

A routine has a duplicate label within it. Labels must be unique within the routine.

<LANGUAGE MISMATCH>

While compiling and inserting code into an existing routine, the current language mode differs from that of the routine.

<LICENSE ALLOCATION
EXCEEDED>

There has been an attempt to exceed the operational user limit imposed on
this instance with the $SYSTEM.License.SetUserLimit(InstanceUserLimit)
API.

<LICENSE LIMIT
EXCEEDED>

<LICENSE SERVER
UNAVAILABLE>

There has been an attempt to exceed the number of users allowed by the active InterSystems IRIS license, either on the current IRIS instance or in total among the set of instances sharing the license.

The license server is unreachable at the moment. Check your network.

<LIST>

An improperly-formed list has been used.

<LOCKLOST>

Some locks once owned by this job have been reset.

<LOGIN INHIBITED>

The system is initializing. No users are permitted to begin work.

<MAGTAPE>

<MAXARRAY>

A magnetic tape operation encountered an error. Check $ZA.

There are too many subscripts at this level.

<MAXINCREMENT>

An attempt to $INCREMENT a variable did not change its value.

<MAX LOCKS>

The maximum lock count (32766) has been exceeded.

<MAXNUMBER>

An arithmetic operation has produced a number larger than the implementation allows.

<MAX ROUTINES>

There are no slots available to allocate to invoke a new routine.

<MAXSCOPE>

There has been an attempt to issue more than 31 levels of New commands.

<MAXSTRING>

Description

There has been an attempt to specify or create a data string longer than the string length limit. Attempting to concatenate strings that would result in a string exceeding this maximum string size results in a <MAXSTRING> error.

<METHOD DOES NOT
EXIST>

The method does not exist in the specified class or the class of the specified
object. For further details, refer to $ZERROR.

<METHOD NOT
SUPPORTED>

The method exists, but is not supported in this context. For example, a nested call to %ToJSON() where the referenced object is not a dynamic object or array.

<MNEMONICSPACE>

There has been an attempt to use control mnemonics for a device with no associated mnemonic space.

<NAKED>

<NAME>

<NAMEADD>

There has been an attempt to use a naked global reference when the naked state was undefined.

There is invalid syntax in a name.

There has been an overflow of device name table, resulting from the Open command.

<NAMESPACE>

The specified namespace is undefined or not active.

<NESTED TOO DEEP>

This error is signaled when processing %DynamicArray and %DynamicObject blocks where the nesting level is too deep.

<NETFORMAT>

<NETGLOREF>

<NETJOBMAX>

<NETLOCK>

<NETRETRY>

<NETSRVFAIL>

There has been an error in a network message. The remote system found fault with the format of a request. Call your support center to resolve this serious error.

There has been an error in a network message. The remote system found fault with the format of a request. Call your support center to resolve this serious error.

Another high-speed networking process cannot be added.This is usually due to an insufficient number of global buffers.

A ObjectScript Lock command has been attempted to a remote computer whose remote system index is greater than 31. To correct, redefine your network configuration to include fewer than 32 remote computers.

An operation failed at the network level in a way that could be immediately retried.

During a transaction COMMIT or a Set, Kill, ZKill command, a client system has detected that one of the servers involved in the transaction has restarted while the transaction was open.

FAILED - BLOCKNUMBER>

An asynchronous network error occurred and updates sent over the network were lost because the remote system attempted to refer to a block that is
outside the bounds of the database; notify your system manager.

FAILED - CLIENT-SERVER
MISMATCH>

An asynchronous network error occurred and updates sent over the network were lost because a network request could not be processed due to incompatibility between the client and server.

Description

FAILED - CLUSTERFAILED>

An asynchronous network error occurred and updates sent over the network were lost because a cluster member failed during global buffer lock processing.

FAILED - DATABASE>

An asynchronous network error occurred and updates sent over the network were lost because InterSystems IRIS on the server has detected degradation
in this database. This is potentially a very serious system error; notify your
system manager.

FAILED - DIRECTORY>

An asynchronous network error occurred and updates sent over the network were lost because the referenced directory is not on the remote system.

FAILED - DISKHARD>

An asynchronous network error occurred and updates sent over the network were lost because InterSystems IRIS on the server has encountered an uncorrectable disk hardware error. This may also be the result of a database
problem; notify your system manager.

FAILED - FILEFULL>

An asynchronous network error occurred and updates sent over the network were lost because InterSystems IRIS on the server has encountered a <FILEFULL> error.

FAILED - MAXSTRING>

An asynchronous network error occurred and updates sent over the network were lost because InterSystems IRIS on the server has encountered an attempt to specify or create a data string longer than the implementation allows (32,767 characters).

FAILED - NETFORMAT>

An asynchronous network error occurred and updates sent over the network were lost because the remote system found fault with the format of a request. Call your support center to resolve this serious error.

FAILED - NETGLOREF>

An asynchronous network error occurred and updates sent over the network were lost because the remote system found fault with the format of a request. Call your support center to resolve this serious error.

FAILED - NETVERSION>

An asynchronous network error occurred and updates sent over the network were lost because the client and server systems are running different ECP versions, which cannot accept each other's message format.

FAILED - PROTECT>

An asynchronous network error occurred and updates sent over the network were lost because a <PROTECT> error occurred.

FAILED - STRINGSTACK>

An asynchronous network error occurred and updates sent over the network were lost because a <STRINGSTACK> error occurred.

FAILED - STRMISMATCH>

An asynchronous network error occurred and updates sent over the network were lost because there has been an internal error handling big strings over the network.

FAILED - SUBSCRIPT>

An asynchronous network error occurred and updates sent over the network were lost because a <SUBSCRIPT> error occurred.

FAILED - SYSTEM>

An asynchronous network error occurred and updates sent over the network were lost because a <SYSTEM> error occurred on the server. Either there has been an attempt to do something not allowed by the operating system or ECP. Or there is an error condition in InterSystems IRIS, in which case you should notify your support center with as much information as possible.

Description

FAILED - WIDECHAR>

An asynchronous network error occurred and updates sent over the network were lost because a <WIDECHAR> error occurred.

FAILED>

An asynchronous network error occurred and updates sent over the network were lost. The reasons for the loss are undetermined. Call your support center to resolve this serious error.

<NETWORK UNLICENSED>

The application has attempted to access a remote directory, but there is no license for InterSystems IRIS networking.

<NETWORK>

<NLS TABLE>

Typically, one of the following has occurred: the network timeout has expired;
the local port has gone down; the node being accessed is down; or the remote
server connection is disabled.

There has been an attempt to perform NLS translation using data that is not proper for the conversion table.

<NO CURRENT OBJECT>

There is no current object.

<NO MAILBOX>

A resource needed for interprocess communication is unavailable.

<NO SOURCE>

A source line is missing from a routine in the routine source global.

<NODEV>

<NOJOB>

There has been an attempt to a write-only device, or write to a read-only device, with interjob communication.

There has been an attempt to specify an incorrect process number in a View command, or an error occurred in a Job command.

<NOLINE>

There has been an attempt to refer to a nonexistent routine line.

<NORESTART>

The application or function cannot be restarted.

<NOROUTINE>

<NOSYS>

There has been an attempt to refer to a nonexistent routine. For further details,
refer to $ZERROR.

There has been an attempt to make an extended or implicit reference to a remote system that is not reachable in the current network configuration. The remote system is not in the tables.

<NOT PRIMARY VOLUME>

Volume sequence is not 1; the volume label disagrees with the function of
the volume.

<NOTOPEN>

The device cannot be opened, or there has been an attempt to use an unopened device.

<NULL VALUE>

A null string appears where one is not allowed.

<OBJECT DISPATCH>

<OUT OF $ZF HEAP
SPACE>

<PARAMETER>

A non-multidimensional object property was supplied to a function that can only take a multidimensional object property. For further details, refer to
$DATA or $GET.

The $ZF heap lacks the necessary available space to support one of the
input or output parameters being passed between InterSystems IRIS and the
external program invoked via the $ZF function.

The number of parameters passed to a labeled line by a user-written function reference or a Do command exceeded the number of formal parameters declared for the labeled line.

Description

<PRIVATE METHOD>

There has been an attempt to invoke a private and, therefore, unavailable method.

<PRIVATE PROPERTY>

There has been an attempt to access a private and, therefore, unavailable property.

<PROPERTY DOES NOT
EXIST>

The property is not part of the class of the specified object. For further details,
refer to $ZERROR.

<PROTECT>

<RANGE>

<READ>

<RECOMPILE>

<REGULAR EXPRESSION>

<REMOTE CLASS EDITED>

<REMOTE CLASS
RECOMPILED>

<REMOTE EXECUTE
INVALID READ>

<REMOTE EXECUTE
INVALID WRITE >

<RESJOB>

<ROLLFAIL>

There has been an attempt to do something with a global (Read, Write, or
Kill) for which there was no authorization; or there has been an attempt to
use a View command which modifies memory, $View, or modifying a
SYS.Database property; or there has been an attempt to use a nonexistent
directory, possibly with extended global syntax, or some other protection violation occurred. A common cause of this error is attempting to write to a database that has been dismounted or to which the user does not have permission to access.

For further details, refer to $ZERROR.

A bit or list position is out of allowable range.

The record cannot be read.

A routine has been compiled under a different version of InterSystems IRIS or an InterSystems legacy product. It cannot be loaded onto this system with %RIMF, which transfers object code. Transfer it as source code (using %RO and %RI) and then recompile it.

There has been an error in the syntax of a regular expression (an invalid or ambiguous regular expression string, or a regular expression that specifies an unimplemented feature).

There has been an attempt to use an object hosted on a remote system whose class has been recompiled from a remote system since the object was created.

There has been an attempt to use an object hosted on a remote system whose class has been recompiled from the local system since the object was created.

A method or routine called from $System.IS.Execute() tried to read from the
current device. This type of I/O is forbidden because it will disrupt the communication channel with the client.

A method or routine called from $System.IS.Execute() tried to write to the
current device. This type of I/O is forbidden because it will disrupt the communication channel with the client.

A process was intentionally terminated.

InterSystems IRIS has encountered an error processing a call to TRollBack. This error means that InterSystems IRIS is not sure whether one or more remote machines actually processed the rollback.

Description

<ROUTINELOAD>

An error occurred in loading a routine. This can indicate that the routine’s OBJ code (object code) is corrupt, which can possibly lead to database degradation. Contact your system manager. This error can also indicate that no routine buffers are available (they are being held by other processes) and the timeout period (roughly 100 seconds) has expired.

<SELECT>

A $Select function contains no true condition.

<SHARED MEM HEAP>

The request for shared memory cannot be satisfied. To avoid this error, the system could be reconfigured with more heap space allocated.

<SLMSPAN>

<STACK>

<STORE>

<STRINGSTACK>

There has been an attempt to kill a global across a subscript level mapping boundary.

The argument stack is out of room or contains an incorrect type.

The process ran out of memory. If you expect the process to use a lot of memory, try doubling or quadrupling the available memory for the process. If the error goes away, then the process needed more memory. If the error persists, then you need to figure out why it is using so much memory. Refer
to the $STORAGE special variable for further details.

An expression is too long, there are too many expressions in an argument for a single command, or an expression contains many very long strings. Simplify the expression.

<STRMISMATCH>

There has been an internal error handling big strings over the network.

<SUBSCRIPT>

<SWIZZLE FAIL>

<SYNTAX>

<SYSTEM>

A subscript has an illegal value or a global reference is too long. For further
details, refer to $ZERROR. For more information on maximum length of global
references, see “ Determining the Maximum Length of a Subscript”.

You have opened an oref and then attempted to swizzle in another related object which could not be referenced. This may be due to the deletion of the related object from disk or another process holding a lock on the related object.

There is a syntax error (an error in the formation of a language construct, such as a misspelled or missing keyword).

Either there has been an attempt to do something not allowed by the operating system, or there is an error condition in InterSystems IRIS, in which case you should notify your support center with as much information as possible.

<TCPWRITE>

A timeout has occurred on a TCP write operation.

<TERMINATOR>

<THROW>

There has been an attempt to read on a terminal or device in image mode with no terminator and it was not a fixed-length read.

A THROW has been issued, but no CATCH expression has been found on
the call stack. For further details, refer to $ZERROR.

<TOO MANY CLASSES>

This process has attempted to access too many active classes.

<TOO MANY LONG
STRINGS>

Too many intermediate long strings are present on the string stack.

<TOO MANY OREFS>

This process has attempted to create too many simultaneously open objects.

Description

<TOO MANY USERS OF
CLASS>

Too many processes are trying to use a particular class simultaneously (more than 65561).

<TOO MANY USERS>

Too many users are attempting to use the system at the same time.

<TOOMANYFILES>

InterSystems IRIS is unable to open a file because the underlying operating system has run out of file descriptors.

<TRANSACTION LEVEL>

The application has too many nested transactions pending.

<TRANSLATE>

<TRANSLOST>

<UNDEFINED>

InterSystems IRIS has read an input value for which it has no translation value. It therefore carries out the Default Action defined on the Translation tab of the InterSystems IRIS NLS utility.

A distributed transaction initiated by this job has been asynchronously rolled back by the server.

There has been a reference to an undefined variable. For further details, refer
to $ZERROR.

<UNIMPLEMENTED>

There has been an attempt to use either an unimplemented function or an unimplemented argument of a legitimate command or function.

<UNIMPLEMENTED
DOUBLE>

<UNKNOWN ERROR>

The use of a floating-point number is not supported in this context.

An unexpected error has occurred. Call your support center to resolve this serious error.

<UNLICENSED>

The available license key does not permit the requested operation, for example, trying to create an encrypted database with an entry license.

<VALUE OUT OF RANGE>

The value is outside the maximum or minimum permissible range.

<VOLUME IS NOT
FORMATTED>

The volume does not have the required formatting.

<VOLUME SET ALREADY
CREATED>

There has been an attempt to format an InterSystems IRIS database that is already formatted.

<WIDE CHAR>

InterSystems IRIS read a multibyte character where a 1-byte character was expected.

<WRITE DEMON FAILED>

The write daemon is unable to continue. Call your support center to resolve this serious error.

<WRITE>

The record cannot be written.

<WRONG NAMESPACE>

There is an attempt to load the class from a private implied namespace.

<ZDDIF>

<ZDPT2>

<ZTRAP>

A DATEDIFF operation was attempted with an invalid date.

A DATEPART operation was attempted with an invalid date.

There has been an attempt to issue a ZTrap command with no argument.

ISO 11756-1999 Standard Errors

### 4.2 ISO 11756-1999 Standard Errors

ObjectScript supports ISO 11756-1999 standard errors. These errors are returned to the $ECODE special variable.

Table 4–2: ISO 11756-1999 Standard Error Messages

Message Text

Meaning

M1

M2

M3

M4

M5

M6

M7

M8

M9

M10

M11

M12

M13

M14

M15

M16

M17

M18

M19

M20

M21

M22

M23

M24

M26

M27

M28

M29

Naked indicator undefined.

Invalid $FNUMBER code string combination.

$RANDOM argument less than 1.

No true condition in $SELECT.

Line reference less than 0 (zero).

Undefined local variable.

Undefined global variable.

Undefined special variable.

Divide by zero.

Invalid pattern match range.

No parameters passed.

Invalid line reference (negative offset).

Invalid line reference (line not found).

Line level not one (1). (DO command.)

Undefined index variable. (FOR command.)

QUIT with an argument not allowed.

QUIT with an argument required.

Fixed-length READ not greater than 0 (zero).

Cannot merge a tree or subtree into itself.

Line must have a formal parameter list.

Formal parameter list name duplication.

SET or KILL to ^$GLOBAL structured system variable name (SSVN) when data in global.

SET or KILL to ^$JOB structured system variable name (SSVN) for nonexistent job
number.

Change to collation algorithm while subscripted local variables defined.

Nonexistent environment (nonexistent namespace).

Attempt to roll back a transaction that is not restartable.

Mathematical function, parameter out of range.

SET or KILL on structured system variable name (SSVN) not allowed by implementation.

Message Text

Meaning

M30

M31

M32

M33

M35

M36

M37

M38

M39

M40

M41

M42

M43

M44

M45

M57

M58

Reference to global variable with different collating sequence within a collating algorithm.

Device control mnemonic expression used for a device without a mnemonic space being selected.

Device control mnemonic used in user-defined mnemonic space which has no associated line.

SET or KILL to ^$ROUTINE when the specified routine exists.

Device does not support mnemonic spaces. (OPEN or USE command.)

Incompatible mnemonic spaces. (OPEN or USE command.)

READ from device identified by null string.

Invalid structured system variable name (SSVN) subscript.

Invalid $NAME argument.

Call by reference in the actual parameter list in JOB command.

Invalid LOCK argument within a transaction.

Invalid QUIT within a transaction.

Invalid range value ($X or $Y). (SET command.)

Invalid command outside a transaction.

Invalid GOTO reference.

A label is defined more than once in a routine.

Too few formal parameters.

The following tables list the error codes associated with productions for InterSystems IRIS® data platform.

### 5.1 Production Errors

Description

ErrAdapterAlreadyConnected

Adapter already connected

ErrBPCancelled

ErrBPCanNotOpen

BusinessProcess cancelled

Can not open BusinessProcess '%1'

ErrBPLASyncTimeoutMustBeOnSync

ASynchronous Call timeout should be specified on <sync> tag, ignored

ErrBPLBadExpressionValue

The indirect expression cannot be evaluated"

ErrBPLEnumeration

'%1' must be in enumeration '%2' for activity '%3'

ErrBPLInvalidContextSuperclass

ErrBPLInvalidLoopContext

ErrBPLLabelNameNotUnique

ErrBPLLabelNotInScope

ErrBPLNodeMissing

ErrBPLNodeValidation

ErrBPLThrownFault

ErrBPTerminated

The context superclass '%1' is invalid as it is NOT a primary subclass of Ens.BP.Context

'%1' cannot be used outside a containing loop construct

Label named '%1' is not unique, branch would be ambiguous

Cannot branch to label '%1' because the label is not in scope

Missing '%1' for activity '%2'

'%1' must NOT be empty string for activity '%2'

%1

Terminating BP %1 #%2 due to error: %3

ErrBusinessDispatchNameNotRegistered

Business dispatch name '%1' is not registered to run

ErrCanNotAcquireJobLock

Can not acquire lock for job registration global in job '%1'

Description

ErrCanNotAcquireJobRootLock

Can not acquire lock for job registration global

ErrCanNotAcquireLaunchLock

Can not acquire lock to run Ens.Job:Launch()

ErrClassNotConcrete

ErrClassNotDefined

ErrClassNotDerived

ErrConfigDisabled

'%1' is not a concrete class

'%1' is not a defined class

'%1' is not derived from class '%2'

Configuration item '%1' is disabled

ErrCredentialsAlreadyExists

"Credentials with name '%1' already exists

ErrDocImport

ErrDTLCannotBeCompiled

ErrDTLEnumeration

ErrDTLNodeValidation

ErrDTSMultiSignature

ErrDTSSignature

ErrException

ErrFailureTimeout

ErrFTPConnectFailed

%1

The DTL contains errors which prevent it from being compiled"

'%1' must be in enumeration '%2' for action '%3'

'%1' must NOT be empty string for action '%2'

Signature error in %1: input %2 does not match '%3'

Signature error in %1: input '%2' does not match '%3'

%1logged as '%2' number %3

FailureTimeout of %1 seconds exceeded in %3; status
from last attempt was %2

FTP: Failed to connect to server '%1' (msg='%2',code=%3)

ErrFTPDeleteFailed

FTP: Failed Delete file '%1' (msg='%2',code=%3)

ErrFTPDirectoryChangeFailed

FTP: Failed to change to directory '%1' (msg='%2',code=%3)

ErrFTPGetDirectoryFailed

FTP: Failed GetDirectory (msg='%1',code=%2)

ErrFTPGetFailed

ErrFTPListFailed

ErrFTPLogoutFailed

ErrFTPModeChangeFailed

ErrFTPNameListFailed

ErrFTPPutFailed

ErrFTPRenameFailed

ErrGeneral

ErrInConnectionLost

ErrInvalidAssign

ErrInvalidBPL

FTP: Failed to Get file '%1' (msg='%2',code=%3)

FTP: Failed List for %1 (msg='%2',code=%3)

FTP: Failed to log out from server '%1' (msg='%2',code=%3)

FTP: Failed to set connection to '%1' mode (msg='%2',code=%3)

FTP: Failed NameList for '%1' (msg='%2',code=%3)

FTP: Failed to Put file '%1' (msg='%2',code=%3)

FTP: Failed Rename file '%1' (msg='%2',code=%3)

%1

Lost %1 connection on %2 - detected via %3

Cannot specify '%1' for '%2'

Invalid BPL

ErrInvalidBPLDiagram

ErrInvalidDateTimeFormat

ErrInvalidDTL

ErrInvalidDurationFormat

ErrInvalidProduction

ErrIWay

ErrIWayNoStatus

ErrJobFailed

ErrJobNotStopped

ErrJobsNotStopped

ErrKeyWithAppend

ErrKeyWithClear

ErrKeyWithInsert

ErrKeyWithRemove

ErrMissingBPL

Production Errors

Description

Invalid BPL Diagram: '%1'

Invalid Date/Time format

Invalid DTL

Invalid Duration format

Invalid Production

iWay XTE Error: %1 (%2)

Failed to find %1 object in iWay XTE response stream '%2'

JOB command failed

Job '%1' failed to stop within %2 seconds

The following jobs failed to stop within %2 seconds:
%1

'key' attribute must not be specified when 'type' attribute is 'append'

'key' attribute must not be specified when 'type' attribute is 'clear'

'key' attribute must be specified when 'type' attribute is 'insert'

'key' attribute must be specified when 'type' attribute is 'remove'

Missing BPL XDATA block, NO EXECUTABLE CODE
GENERATED

ErrNoCallerCredentials

"No Credentials property present in %1

ErrNoClassname

ErrNoCredentials

-no Classname given-

Unable to find Credentials for ID name '%1'

ErrNoCredentialsSystemName

Unable to find Credentials for ID name '%1' : %2

ErrNoElementContent

Cannot Find Element Content

ErrNoFileFound

ErrNoMsgBody

ErrNoObjFromStream

ErrNoObjFromString

ErrNoRawInputObj

ErrNoResponseClass

ErrNoSQLColumns

ErrNoSQLCursor

No File Found

MessageBody does not exist for MessageHeader
#%1

Failed to find element %1 / class %2 in Stream '%3'

Failed to find element %1 / class %2 in String '%3'

No raw input object for '%1'

No Response Classname is assigned for Request class %1

No Columns in Query '%1'

No open Cursor for Query '%1'

ErrNoSQLStatement

ErrNotImplemented

ErrNotRetryable

ErrNoWebProtocol

ErrObjectAlreadyExists

ErrOutConnectException

ErrOutConnectExpired

ErrOutConnectFailed

ErrOutConnectionLost

ErrOutNotConnected

ErrParameterInvocationInvalid

Description

No Statement Executed for Query '%1'

Method %1.%2() not implemented

Non-Retryable %3 error (%2) received after %1 seconds

Unsupported Protocol '%1'

Object ID '%1' of class '%2' already exists

Exception occured while making %2 connection %3 : %1

%2 Connect timeout period (%1) expired for %3

%2 Connect failed for %3 with error %1

Lost %1 connection to %2 - detected via %3

%1 connection to %2 is not open in %3

Parameter value for INVOCATION in invalid in class '%1'

ErrProductionAlreadyRunning

Production '%1' is already running

ErrProductionMismatchInDeferredResponse

ErrProductionNetworkedMismatch

Production name mismatch while sending deferred response

Production '%1' is already running on a different machine in the network, a production of a different name can not be started

ErrProductionNotQuiescent

InterSystems IRIS can not become quiescent

ErrProductionNotRegistered

Failed to open Production definition '%1': %2

ErrProductionNotRunning

No production is running

ErrProductionNotShutdownCleanly

Production '%1' was not shutdown cleanly

ErrProductionQuiescent

InterSystems IRIS is in quiescent state

ErrProductionSettingInvalid

Production setting '%2' for item '%1' is invalid

ErrProductionSuspendedMismatch

Production '%1' was suspended, a new production of a different name can not be started

ErrRequestNotHandled

Request message '%1' not handled

ErrRetryable

ErrRulesetLoadFailed

ErrRulesetNotFound

ErrSOAPConfigClass

ErrSOAPConfigName

Retryable %3 error (%2) received after %1 seconds

RuleSet %1 failed to load: %2

RuleSet %1 cannot be found

SOAP service %1 cannot be invoked because its associated class must match. The class found is %2

SOAP class %1 cannot be invoked because there is no Ensemble service configured with this name

ErrSOAPConfigType

ErrSQLParmCount

ErrSuspending

ErrTCPListen

ErrTCPReadBlockSize

ErrTCPReadBlockSizeTimeoutExpired

ErrTCPReadTimeoutExpired

ErrTCPTerminatedReadTimeoutExpired

ErrTelnetConnectFailed

ErrTelnetFindFailed

ErrTelnetLoginFailed

ErrTerminate

Workflow Errors

Description

SOAP service %1 cannot be invoked because it is not a service

Execute called with a different number of input parameters (%1) than SQLDescribeParameters() demands (%2)

Suspending message %1 as requested by message handler with status: %2

Unable to open TCP/IP port %1 within timeout %2 - Details: %3

TCP Read(%2) with timeout period (%1) failed with :
%3

TCP block size Read (%2) timeout period (%1) expired

TCP Read(%2) timeout period (%1) expired (charset='%3')

TCP Read timeout (%1) expired waiting for terminator %2, data received ='%3'

Telnet: Failed to connect to Telnet server at %1, error code %2

Telnet: Failed to find %1 string(s): '%2', status %3)

Telnet: Login attempt to %1 resulted in Failure Notice '%2'

InterSystems IRIS system termination request detected

ErrUnsupportedRequestType

Request type %1 is not in %2 signature '%3'

ErrValueWithClear

ErrValueWithRemove

'value' attribute must not be specified when 'type' attribute is 'clear'

'value' attribute must not be specified when 'type' attribute is 'remove'

ErrXDataBlockNotDefined

XDATA block '%2' is not defined in class '%1'

ErrXMLValidation

Error in XML Validation: %1 %2

### 5.2 Workflow Errors

ErrGeneral

ErrNoRoleSet

ErrNoUserSet

Description

%1

Unable to create RoleSet

Unable to create UserSet

ErrNoUsersFound

ErrRoleUndefined

ErrTaskAlreadyAssigned

ErrTaskAssignedToOther

ErrTaskCreateFailure

ErrTaskWrongType

ErrUserUndefined

Description

No Users found to assign Task to

Workflow Role '%1' not defined

Task '%1' is already assigned

Task '%1' is assigned to another User '%2'

Unable to create TaskHandler '%1'

TaskHandler class '%1' has wrong Type

Workflow User '%1' not defined

### 5.3 XPATH Transformation Errors

XPathDOMResult

XPathMultipleResults

XPathNOResult

Description

XPath dom result returned when single value requested

XPath expression evaluation returned multiple results

XPath expression evaluation didn't return any results

### 5.4 Electronic Data Interchange (EDI) Errors

ErrMapBuild1

ErrMapBuilds

ErrMapDocType

ErrMapRequired

Description

%1 BuildMap error: %2

%1 BuildMap errors; first: %2

No %1 schema structure is defined for DocType '%2'

Missing required %1 element at segment %2

ErrMapRequiredUnion

Missing required %1 union element at segment %2

ErrMapSeg

ErrMapSegCount

ErrMapSegUnrecog

ErrMapWildSegUnrecog

ErrMapWildSegUnrecogAfterWild

Segment '%1' not mapped by schema

Missing required count (%1) for %2 segment at segment %3

Unrecognized Segment %1 found after segment %2

Unrecognized '%3' Segment %1 found after segment %2

Unrecognized '%3' Segment %1 found after '%3' segment %2

InvalidCategoryName

InvalidDocType

Category name %1 is invalid

Document Type '%1' is invalid

HL7 Version 2 Message Routing Errors

Description

InvalidDocumentTypeName

Document Type Name '%1' is invalid

InvalidSegmentTypeName

UnknownCategoryName

UnknownDocumentTypeName

Segment name '%1' is invalid

Category name %1 is unknown

Document Type Name '%2' not found in Document Category '%1'

UnknownSegmentTypeName

Segment name '%1' is unknown

### 5.5 HL7 Version 2 Message Routing Errors

ErrAckSeqNum

ErrEndBlock

ErrGeneral

ErrStartBlock

Description

ACKing to MSH Sequence Number query

Received unexpected EndBlock '%1' in input "%3", expected '%2'

%1

Received unexpected StartBlock '%1' in input "%3", expected '%2'

### 5.6 X12 Standard Exchange Format (SEF) File Errors

FileNotExists

NodeEnumeration

NodeIdentification

NodeValidation

Description

EDI Schema definition file '%1' does not exist

%1 value '%2' is not in enumeration %3

for Node %1

%1 value '%2' Failed Validation for Node %3

### 5.7 X12 Message Routing Errors

BadBINLength

BinaryLeftover

CannotDetermineSchema

Description

Binary '%1' segment contains invalid length value %2;
text:'%3'

Binary segment contains extra text '%1'

Schema cannot be determined, cannot validate transaction

CannotRespond

ConstraintViolation

ControlSegment

Description

Not enough data to Generate valid Response

Constraint violated: %1 for: %2

Referenced SetSegment %1 is a Control Segment

ControlSegmentNameMandatory

Control Segment name must NOT be empty

ControlVersionUnsupported

DuplicateControlNumber

This control version: %1 NOT supported; found in
segment %2 at %3

Duplicate Interchange Control Number %1 for Sender %2

DuplicateSegmentRef

Segment reference: %1 already exists

DuplicateTSControlNumber

Duplicate Transaction Set Control Number %1

ElementNameMandatory

Element Name cannot be empty

EmptyElement

ExistsElement

ExpectedDelimiter

ExpectedSegment

Element %1 has empty value in segment %2

Element %1 has non-empty value in segment %2

Expected delimiter Ascii %1; in segment %2

Expected %1 Segment; got segment: '%2' at %3

FatalInterchangeError

Fatal X12 Interchange Error

GroupControlNumberMismatch

ImplementationKeyReserved

IncorrectFunctionalGroupCount

IncorrectSegmentCount

IncorrectTransactionCount

IndexImmutable

InterchangeControlNumberMismatch

Group control number mismatch, expected %1, received %2 at: %3

Implementation key ISC reserved for use by
InterSystems Corporation

Number of included functional groups mismatch, expected: %1 tallied: %2 at: %3

Number of included segments count is incorrect, expected %1, tallied: %2 at: %3

Number of included transactions count is incorrect, expected %1, tallied: %2 at: %3

Cannot modify by index, current access mode is by
Path

Interchange control number mismatch, expected %1, received %2 at: %3

InvalidCode

Element %1, code value %2 at segment: %3 is invalid

InvalidComponentReference

Invalid component in path %1

InvalidComponentSeparator

InvalidCompositeElement

InvalidDataSeparator

Invalid Component Element separator 'Ascii %1' in segment '%2' at %3

Composite Element: %1 is not valid for segment: %2

Invalid Data Element separator 'Ascii %1' in segment:
at '%2' at %3

InvalidExponent

InvalidHSC

InvalidIndex

InvalidItemName

InvalidItemReference

InvalidNumericValue

InvalidPropertyPath

InvalidRepetitionSeparator

InvalidSegmentItem

InvalidSegmentName

InvalidSegmentRef

InvalidSegmentTerminator

InvalidSegmentType

InvalidType

ISATruncated

ItemNotBinary

ItemNotComposite

MaxIndex

NotUsedHasValue

PathImmutable

SchemaUnresolved

SegmentDoesNotExist

SegmentImmutable

SegmentRuleViolated

TA1OrGroupNotSeen

TA1ValidationFailed

X12 Message Routing Errors

Description

Exponent Is Invalid must be signed/unsigned integer

Invalid Hierarchical Structure Code %1

Invalid syntax in property path %1, repetition index MUST be 1 or Greater

Item name %1 is invalid

Invalid item in path %1

Invalid numeric value: %1

Property Path %1 is invalid

Invalid Repetition separator 'Ascii %1' in segment '%2' at %3

Item: %1 is not valid for segment: %2

Segment name '%1' in segment '%2' is badly formed at %3

Segment reference: %1 is invalid

Invalid Segment Terminator 'Ascii %1' in segment '%2' at %3

Segment Type %1 at %3 not allowed after segment %2

Both Category and TName must be present in transaction set type reference %1

Interchange Header (ISA) segment is too short (must
be 106 characters); found %1:'%2' at %3

Referenced Item: %1 is NOT binary

Referenced Item: %1 is NOT a Composite Value

Repetition index exceeds maximum : %1

Item %1 marked as not used, has non-empty value

Cannot modify by path, current access mode is by
Index

Cannot use path as schema is unresolved (DocType Not Set or Invalid?)

Segment ID: % does not exist in segment storage

Segment is immutable, cannot modify

%1 Rule %2 Violated for segment: %3

At Least one TA1 Acknowledgment or Functional Group MUST be included in Interchange

Validation error occured for element: %1 of TA1 acknowledgment at segment: %2

TransactionImmutable

TransactionSetControlNumberMismatch

UnexpectedElement

UnexpectedEOD

UnexpectedEODBinary

Description

TransactionSet is immutable, cannot modify

TransactionSet control number mismatch, expected %1 received: %2 at: %3

Too many elements in segment %1

Unexpected end of data after '%1' at %2 : %3

Unexpected end of data while reading binary segment after '%1'

UnhandledTA1

Unable to process TA1 see segment: %2

UnknownBinarySegment

Unrecognized Binary Segment %1

UnknownComponentReference

Unrecognized component in path %1

UnknownElementName

Unrecognized element name: %1

UnknownHSC

UnknownItemName

Unrecognized Hierarchical Structure Code %1

Item name %1 is unknown

UnknownItemReference

Unrecognized item in path %1

UnknownSender

ValidateComposite

Unknown Sender

Cannot Validate Composite

### 5.8 DICOM Message Routing Errors

Description

AbstractSyntaxNotSupported

Abstract Syntax '%1' is NOT supported

BadCalledAET

BadCallingAET

BadCharacter

BadTagValue

ContextMismatch

The Called AET has an invalid value

The Calling AET has an invalid value

Expected '%1' found '%2' in property reference '%3' at position '%4'

The tag value '%1' must be 4 hexadecimal digits in property reference '%2' at position '%3'

Context IDs in successive message fragments don't match

ConvertToXML

Unable to convert DICOM file '%1' to XML

DataElementIsNotASequence

DataSetTagNotFound

DataValueFailsVMConstraint

The dataelement '%1' in property reference '%2' at position '%3' is not of type 'sequence'

The 'data-set' element could not be found in the XML meta data

Data Value for Tag '%1' failed value multiplicity constraint '%2'

DICOM Message Routing Errors

Description

DataValueFailsVRConstraint

FileNameForStoreOperationNotFound

InvalidFileFormat

InvalidMessageHandler

Data Value for Tag '%1' failed value representation constraint '%2'

The file name for the C-STORE operation could not be determined

The Dicom File appears to contain invalid data

Message Handler must be a type of
EnsLib.DICOM.Util.MessageHandler

InvalidPropertyReference

The property reference '%1' is invalid

MaxAbstractSyntaxCountExceeded

Maximum number of Abstract Syntax items exceeded

MaxApplicationContextCountExceeded

MaxReceivedPDULen

Maximum number of Application Context items exceeded

Received PDU Length of '%1' exceeds negotiated maximum of '%2'

MaxTransferSyntaxCountExceeded

Maximum number of Transfer Syntax items exceeded

MaxUserInfoItemsExceeded

Maximum number of User Information items exceeded

MaxUserMaxLenCountExceeded

Maximum number of User MaxLen items exceeded

MessageIncomplete

NoActiveAssociation

NoAssociationDefinedForPeers

PDUBadLength

PDUBadValue

PeerAssociationNoLongerExists

PeerClosedConnection

PeerRejectedAssociation

PeerRequestedAbort

SyntaxError

Message '%1' - incomplete

No Active Association for Calling-AET '%1 and Called-AET '%2'

No AssociationContext defined for Calling-AET '%1' and Called-AET '%2'

PDU '%1' - Item '%2', expected length of '%3', received '%4'

PDU '%1' - Item '%2', expected value of '%3', received '%4'

The Peer Association no longer exists, processing cannot continue

Peer closed connection

Peer rejected Association. Source: '%1' Reason: '%2'

Peer requested Association Abort

Syntax Error in property reference '%1' at position '%2'

TimedOutWaitingForResponseFromPeer

Timed-out waiting for response from peer

TransferSyntaxNotSupported

Transport Syntax '%1' is NOT supported

UnableToConnectToPeer

UnableToCreateDirectory

UnexpectedAdapterState

Unable to Connect to peer '%1 : '%2'

UnableToCreateDirectory '%1'

Protocol Error, Adapter is in state '%1', expected state '%2'

UnexpectedItem

UnexpectedMessage

UnexpectedPDU

Description

Unexpected Item type '%1' received

Unexpected Message type '%1' received

Unexpected PDU type '%1' received

UnrecognizedAbstractSyntax

The Abstract Syntax '%1' is unrecognized

UnrecognizedCommandReceived

Unrecognized (unsupported) command '%1' received

UnrecognizedDataElement

The Data Element '%1' is unrecognized

UnrecognizedTag

The Data Element Tag '%1' is unrecognized

UnrecognizedTransferSyntax

The Transfer Syntax '%1' is unrecognized

VDOCPropertyDoesNotExist

The property specified by property path %1 does not exist
