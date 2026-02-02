# Configuration P arameter File Reference

Parameter File

When InterSystems IRIS® data platform starts, it reads configuration v alues from the configur ation parameter file (CPF), iris.cpf. This file defines the particular InterSystems IRIS configuration for each instance.

This topic discusses how to use and edit the CPF. The Table of Contents at the beginning of this reference shows each parameter in the CPF, sorted by section.

Note: Within this document and the CPF itself, memory is depicted as powers of two. For example, a kilobyte (KB)

means 1024 bytes, and a megabyte (MB) means 1024 KB.

### 1.1 CPF Overview

The configuration parameter file, also called the CPF , defines an InterSystems IRIS Configur ation. On startup, InterSystems IRIS reads the CPF to obtain the values for most of its settings.

The default CPF, iris.cpf, is located in the installation directory. There are multiple ways to modify the CPF, as described in Editing the Active CPF.

InterSystems IRIS creates multiple backups of the CPF. Once per day, if the iris.cpf file is modified, InterSystems IRIS creates a backup in the same directory called iris.cpf_yyyymmdd. These backups are automatically purged after one year. Additionally, after a successful startup or shutdown, a copy of the CPF is saved in the installation directory as _LastGood_.cpf, which represents the most recent valid configuration.

#### 1.1.1 CPF Format

A configuration parameter file is a line-oriented, UTF-8 te
and line feed. Long items cannot be continued on a following line. A line in the file is one of the follo wing elements:

.cpf extension. Each line ends with a carriage return

xt file with a

- Blank Space – An empty line made up of zero or more spaces.

- Section Heading – The name of a file section enclosed in square brack ets [].

- Parameter – An InterSystems IRIS configuration parameter and its v alue(s).

- Comment – A comment added by a user.

##### 1.1.1.1 Blank Space

In general, spaces at the beginning and end of lines are without effect. Spaces within the line are usually significant. The best practice is to use no spaces in the line except where they are meaningful components of strings.

##### 1.1.1.2 Section Headings

Related settings are collected into sections. The beginning of a section is marked by a line consisting of the name of the
section enclosed in square brackets. For example:

[Devices]

All lines after the section heading, up to the next section name (or the end of file), are in the same section.

##### 1.1.1.3 Parameters

Each line beneath a section heading is the definition of a parameter . Each parameter line uses the following syntax, where
keyword is a parameter name and value is a string:

keyword=value

When there is a set of similar items to configure, the y may be displayed as keyword_#. Examples include namespaces, databases, devices, and anything else of which there is a group or set of similar items to configure, one per line. The syntax
is:

keyword_1=value keyword_2=value keyword_n=value

The syntax for the value string varies widely from parameter to parameter. The string may indicate true or false using 1 or
0; it may be a number of bytes, or a number of megabytes; it may be a single value, or it may contain multiple values sep-
arated by a delimiter character on the same line. If there is a delimiter within the string, it may be a comma, semicolon, tilde (~), slash (/), colon, or some combination of these, depending on the parameter.

Important:

Beginning with version 2022.1.1 of InterSystems IRIS, setting a parameter equal to empty quotes (parameter="") sets its value to the empty string, or no value. (In previous versions, doing so set the parameter to its default value.) You cannot set a required parameter to the empty string, so setting such a parameter to empty quotes results in an error.

##### 1.1.1.4 Comments

The CPF supports comments. These can appear on a single line or across multiple lines. Comments can start at the beginning of the line or after other content on a line.

To introduce a single-line comment use “;” (semicolon), “#” (pound sign), or “// ” (two slashes).

To introduce a multiline comment, use “/*” (slash, asterisk) to begin the commend and “*/ ” (asterisk, slash) to end it.

#### 1.1.2 Sample CPF

A sample default CPF, installed on a Windows system as part of InterSystems IRIS version 2025.3 with an auto-configured web server, is shown in the following. Note, for example, the default superserver and web server ports (1972 and 80, respectively) in the values for DefaultPort and WebServerPort parameters in the Startup section.

[ConfigFile]
Product=IRIS
Version=2025.3

[Databases]
IRISSYS=C:\InterSystems\IRIS20253\mgr\
IRISSECURITY=C:\InterSystems\IRIS20253\mgr\irissecurity\
IRISLIB=C:\InterSystems\IRIS20253\mgr\irislib\
IRISTEMP=C:\InterSystems\IRIS20253\mgr\iristemp\
IRISLOCALDATA=C:\InterSystems\IRIS20253\mgr\irislocaldata\
IRISAUDIT=C:\InterSystems\IRIS20253\mgr\irisaudit\
ENSLIB=C:\InterSystems\IRIS20253\mgr\enslib\
IRISMETRICS=C:\InterSystems\IRIS20253\mgr\irismetrics\
USER=C:\InterSystems\IRIS20253\mgr\user\

[Namespaces]
%SYS=IRISSYS
USER=USER

[MirrorMember]
AgentAddress=
AsyncMemberGUID=
AsyncMemberType=0 AsyncUseSystemPurgeInterval=0 JoinMirror=0
SystemName=
ValidatedMember=0
VirtualAddressInterface=

[Journal]
AlternateDirectory=C:\InterSystems\IRIS20253\mgr\journal\
ArchiveName=
BackupsBeforePurge=2 CompressFiles=1
CurrentDirectory=C:\InterSystems\IRIS20253\mgr\journal\
DaysBeforePurge=2 FileSizeLimit=1024 FreezeOnError=0
JournalFilePrefix=
JournalcspSession=0 PurgeArchived=0

[Startup]
CallinHalt=1 CallinStart=1
CliSysName=
DBSizesAllowed=8192 DefaultPort=1972
DefaultPortBindAddress=
EnableVSSBackup=1 EnsembleAutoStart=1 ErrorPurge=30 FIPSMode=0 IPv6=0 JobHalt=1 JobServers=0 JobStart=1
LicenseID=
MaxConsoleLogSize=5 MaxIRISTempSizeAtStart=0
PasswordHash=
ProcessHalt=1 ProcessStart=1 ShutdownTimeout=300 SystemHalt=1
SystemMode=
SystemPerformanceDailyReportsOn=0 SystemPerformanceDailyReportsRetentionDays=60 SystemStart=1
TempDirectory=Temp
TerminalPrompt=8,2 WebServer=0
WebServerName=
WebServerPort=80 WebServerProtocol=http
WebServerSSLConfiguration=
WebServerURLPrefix=iris20253 ZSTU=1

[WorkQueues]
Default=
SQL=
Utility=,,,,1

[Logging]
ChildProcessLaunchCommand=irislogd.exe -f /tmp/irislogd.log Enabled=0
EventFilter=
Format=NVP

Interval=10
Level=WARN

[Gateways]
%DotNet Server=.NET,53372,%Gateway_Object,N6.0
%IntegratedML Server=ML,53572,%Gateway_ML
%JDBC Server=JDBC,53772,%Gateway_SQL,,,,,,,0
%Java Server=Java,53272,%Gateway_Object
%ODBC Server=ODBC,53972,%Gateway_SQL
%Python Server=Python,53472,%Gateway_Object
%R Server=R,53872,%Gateway_Object
%XSLT Server=XSLT,53672,%Gateway_Object,,,,,,,0

[DeviceSubTypes]
C-ANSI=80^#,$C(27,91,72,27,91,74)^25^$C(8)^W $C(27,91)_(DY+1)_";"_(DX+1)_"H" S
$X=DX,$Y=DY^$C(27,91,74)^$C(27,91,75)^$C(27,91,72,27,91,74)^$C(8,32,8)
C-IRIS Terminal=80^#,$C(27,91,72,27,91,74)^24^$C(8)^W $C(27,91)_(DY+1)_";"_(DX+1)_"H" S
$X=DX,$Y=DY^$C(27,91,74)^$C(27,91,75)^$C(27,91,72,27,91,74)^$C(8,32,8)
C-TV925=80^#,$C(27,44)^24^$C(8)^W $C(27,61,DY+32,DX+32) S $X=DX,$Y=DY^^^$C(27,44)^$C(8,32,8)
C-VT100=80^#,$C(27,91,72,27,91,74)^24^$C(8)^W $C(27,91)_(DY+1)_";"_(DX+1)_"H" S
$X=DX,$Y=DY^$C(27,91,74)^$C(27,91,75)^^
C-VT101W=132^#,$C(27,91,72,27,91,74)^14^$C(8)^W $C(27,91)_(DY+1)_";"_(DX+1)_"H" S
$X=DX,$Y=DY^$C(27,91,74)^$C(27,91,75)^^
C-VT132=132^#,$C(27,91,72,27,91,74)^24^$C(8)^W $C(27,91)_(DY+1)_";"_(DX+1)_"H" S
$X=DX,$Y=DY^$C(27,91,74)^$C(27,91,75)^^
C-VT220=80^#,$C(27,91,72,27,91,74)^24^$C(8)^W $C(27,91)_(DY+1)_";"_(DX+1)_"H" S
$X=DX,$Y=DY^$C(27,91,74)^$C(27,91,75)^$C(27,91,72,27,91,74)^$C(8,32,8)
C-VT240=80^#,$C(27,91,72,27,91,74)^24^$C(8)^W $C(27,91)_(DY+1)_";"_(DX+1)_"H" S
$X=DX,$Y=DY^$C(27,91,74)^$C(27,91,75)^$C(27,91,72,27,91,74)^$C(8,32,8)
C-VT52=80^#,$C(27,72)^24^$C(8)^W $C(27,89,DY+32,DX+32) S $X=DX,$Y=DY^^^^
M/UX=255^#^66^$C(8)^^^^^
MAIL=132^#^11^$C(8)^^^^^
P-DEC=132^#^66^$C(8)^^^^^
PK-DEC=150^#^66^$C(8)^^^^^
PK-QUME=150^#^66^$C(8)^^^^^

[Devices]
0=0^TRM^C-IRIS Terminal^^^^Principal device^
2=2^SPL^PK-DEC^^^^Spool LA120^
47=47^MT^M/UX^^("auv":0:2048)^^Magnetic tape^ 48=48^MT^M/UX^^("avl":0:2048)^^Magnetic tape^ 57=57^BT^M/UX^^("auv":0:2048)^^Magnetic tape^ 58=58^BT^M/UX^^("avl":0:2048)^^Magnetic tape^
SPOOL=2^SPL^PK-DEC^^^^Spool LA120^
TERM=0^TRM^C-IRIS Terminal^^^^Windows Console^
|LAT|=0^TRM^C-VT220^^^^Principal device^
|PRN|=|PRN|^OTH^P-DEC^^"W"^^Windows Printer^
|TNT|=0^TRM^C-VT220^^^^Principal device^
|TRM|=0^TRM^C-IRIS Terminal^^^^Windows Console^

[MagTapes]
47=\\.\TAPE0 48=\\.\TAPE1 57=\\.\TAPE0 58=\\.\TAPE1

[config]
LibPath=
MaxServerConn=1 MaxServers=2
Path=
PythonPath=
PythonRuntimeLibrary=
PythonRuntimeLibraryVersion=
UUIDv1RandomMac=0 bbsiz=-1 console=, errlog=500 globals=0,0,0,0,0,0 gmheap=0 history=500 ijcbuff=512 ijcnum=16 jrnbufs=64 locksiz=0 memlock=0 netjob=1 nlstab=50
overview=Windows (Intel)~Windows
pijdir= routines=0 targwijsz=0 udevtabsiz=24576 wijdir= zfheap=0,0

[Miscellaneous]
AsyncDisconnectErr=0 AsynchError=1 BreakMode=1 CollectResourceStats=0 DisconnectErr=0 FileMode=0 GlobalKillEnabled=1 IEEEError=1 LicenseAltHeaders=0 LineRecall=1 ListFormat=1 LogRollback=0 MVDefined=0 NodeNameInPid=0 NullSubscripts=0 OldZU5=0 OpenMode=0 PopError=0 RefInKind=0 ScientificNotation=1 SetZEOF=0 ShutDownLogErrors=0 StopID=0 SwitchOSdir=0 SynchCommit=0 TelnetNUL=0 TruncateOverflow=0 Undefined=0 UseNagleAlgorithm=0 ViewPastData=0 ZDateNull=0 ZaMode=0

[ECP]
ClientReconnectDuration=1200 ClientReconnectInterval=5 ServerTroubleDuration=60

[Cluster]
CommIPAddress=
JoinCluster=0

[LicenseServers]
LOCAL=127.0.0.1,4002

[Monitor]
OTELInterval=10
OTELLogLevel=WARN
OTELLogs=0 OTELMetrics=0 SNMPEnabled=0

[IO]
File=^%X364
MagTape=^%XMAG
Other=^%X364
Terminal=^%X364

[SQL]
ANSIPrecedence=1 AdaptiveMode=1 AllowRowIDUpdate=0 AutoParallel=1 AutoParallelThreshold=3200 AutoStatsForEfficientSamplingTableOnly=1 AutoStatsForFixedStatsTable=1 AutoStatsForRemoteGlobalTable=0 BiasQueriesAsOutlier=1 ClientMaxIdleTime=0 Comment=1 DDLDefineBitmapExtent=1 DDLFinal=1 DDLNo201=0 DDLNo30=0 DDLNo307=0 DDLNo311=0 DDLNo315=0 DDLNo324=0 DDLNo333=0 DDLSQLOnlyCompile=0 DDLUseExtentSet=1 DDLUseSequence=1
DefaultSchema=SQLUser

DelimitedIds=1 DropDelete=1 ECPSync=0 ExtrinsicFunctions=0 FastDistinct=1 IdKey=1
IdTrxFrom=~ `!@#$%^&*()_+-=[]\{}|;':",./<>?
IdTrxTo=
LockThreshold=1000 LockTimeout=10 ODBCVarcharMaxlen=4096 ParameterSampling=0 QueryProcedures=0 RTPC=1 ReferentialChecks=1 SaveMAC=0 TCPKeepAlive=300 TODATEDefaultFormat=DD MON YYYY TimePrecision=0

[SqlSysDatatypes]
BIGINT=%Library.BigInt
BIGINT(%1)=%Library.BigInt
BINARY=%Library.Binary(MAXLEN=1)
BINARY VARYING=%Library.Binary(MAXLEN=1)
BINARY VARYING(%1)=%Library.Binary(MAXLEN=%1)
BINARY(%1)=%Library.Binary(MAXLEN=%1)
BIT=%Library.Boolean
BLOB=%Stream.GlobalBinary
CHAR=%Library.String(MAXLEN=1)
CHAR VARYING=%Library.String(MAXLEN=1)
CHAR VARYING(%1)=%Library.String(MAXLEN=%1)
CHAR(%1)=%Library.String(MAXLEN=%1)
CHARACTER=%Library.String(MAXLEN=1)
CHARACTER VARYING=%Library.String(MAXLEN=1)
CHARACTER VARYING(%1)=%Library.String(MAXLEN=%1)
CHARACTER(%1)=%Library.String(MAXLEN=%1)
CLOB=%Stream.GlobalCharacter
DATE=%Library.Date
DATETIME=%Library.DateTime
DATETIME2=%Library.DateTime
DEC=%Library.Numeric(MAXVAL=999999999999999,MINVAL=-999999999999999,SCALE=0)
DEC(%1)=%Library.Numeric(MAXVAL=<|'$$maxval^%apiSQL(%1,0)'|>,MINVAL=<|'$$minval^%apiSQL(%1,0)'|>,SCALE=0)
DEC(%1,%2)=%Library.Numeric(MAXVAL=<|'$$maxval^%apiSQL(%1,%2)'|>,MINVAL=<|'$$minval^%apiSQL(%1,%2)'|>,SCALE=%2)
DECIMAL=%Library.Numeric(MAXVAL=999999999999999,MINVAL=-999999999999999,SCALE=0)
DECIMAL(%1)=%Library.Numeric(MAXVAL=<|'$$maxval^%apiSQL(%1,0)'|>,MINVAL=<|'$$minval^%apiSQL(%1,0)'|>,SCALE=0)
DECIMAL(%1,%2)=%Library.Numeric(MAXVAL=<|'$$maxval^%apiSQL(%1,%2)'|>,MINVAL=<|'$$minval^%apiSQL(%1,%2)'|>,SCALE=%2)
DOUBLE=%Library.Double
DOUBLE PRECISION=%Library.Double
EMBEDDING(%1,%2)=%Library.Embedding(MODEL=%1,SOURCE=%2)
FLOAT=%Library.Double
FLOAT(%1)=%Library.Double
IMAGE=%Stream.GlobalBinary
INT=%Library.Integer(MAXVAL=2147483647,MINVAL=-2147483648)
INT(%1)=%Library.Integer(MAXVAL=2147483647,MINVAL=-2147483648)
INTEGER=%Library.Integer(MAXVAL=2147483647,MINVAL=-2147483648)
LONG=%Stream.GlobalCharacter
LONG BINARY=%Stream.GlobalBinary
LONG RAW=%Stream.GlobalBinary
LONG VARCHAR=%Stream.GlobalCharacter
LONG VARCHAR(%1)=%Stream.GlobalCharacter
LONGTEXT=%Stream.GlobalCharacter
LONGVARBINARY=%Stream.GlobalBinary
LONGVARBINARY(%1)=%Stream.GlobalBinary
LONGVARCHAR=%Stream.GlobalCharacter
LONGVARCHAR(%1)=%Stream.GlobalCharacter
MEDIUMINT=%Library.Integer(MAXVAL=8388607,MINVAL=-8388608)
MEDIUMINT(%1)=%Library.Integer(MAXVAL=8388607,MINVAL=-8388608)
MEDIUMTEXT=%Stream.GlobalCharacter
MONEY=%Library.Currency
NATIONAL CHAR=%Library.String(MAXLEN=1)
NATIONAL CHAR VARYING=%Library.String(MAXLEN=1) NATIONAL CHAR VARYING(%1)=%Library.String(MAXLEN=%1)
NATIONAL CHAR(%1)=%Library.String(MAXLEN=%1)
NATIONAL CHARACTER=%Library.String(MAXLEN=1)
NATIONAL CHARACTER VARYING=%Library.String(MAXLEN=1) NATIONAL CHARACTER VARYING(%1)=%Library.String(MAXLEN=%1)
NATIONAL CHARACTER(%1)=%Library.String(MAXLEN=%1)
NATIONAL VARCHAR=%Library.String(MAXLEN=1)
NATIONAL VARCHAR(%1)=%Library.String(MAXLEN=%1)
NCHAR=%Library.String(MAXLEN=1)
NCHAR(%1)=%Library.String(MAXLEN=%1)
NTEXT=%Stream.GlobalCharacter
NUMBER=%Library.Numeric(SCALE=0)
NUMBER(%1)=%Library.Numeric(MAXVAL=<|'$$maxval^%apiSQL(%1)'|>,MINVAL=<|'$$minval^%apiSQL(%1)'|>,SCALE=0)

Editing the Active CPF

NUMBER(%1,%2)=%Library.Numeric(MAXVAL=<|'$$maxval^%apiSQL(%1,%2)'|>,MINVAL=<|'$$minval^%apiSQL(%1,%2)'|>,SCALE=%2)
NUMERIC=%Library.Numeric(MAXVAL=999999999999999,MINVAL=-999999999999999,SCALE=0)
NUMERIC(%1)=%Library.Numeric(MAXVAL=<|'$$maxval^%apiSQL(%1,0)'|>,MINVAL=<|'$$minval^%apiSQL(%1,0)'|>,SCALE=0)
NUMERIC(%1,%2)=%Library.Numeric(MAXVAL=<|'$$maxval^%apiSQL(%1,%2)'|>,MINVAL=<|'$$minval^%apiSQL(%1,%2)'|>,SCALE=%2)
NVARCHAR=%Library.String(MAXLEN=1)
NVARCHAR(%1)=%Library.String(MAXLEN=%1)
NVARCHAR(%1,%2)=%Library.String(MAXLEN=%1)
NVARCHAR(MAX)=%Stream.GlobalCharacter
POSIXTIME=%Library.PosixTime
RAW(%1)=%Library.Binary(MAXLEN=%1)
REAL=%Library.Double
ROWVERSION=%Library.RowVersion
SERIAL=%Library.Counter
SMALLDATETIME=%Library.DateTime(MINVAL="1900-01-01 00:00:00",MAXVAL="2079-06-06 23:59:59")
SMALLINT=%Library.SmallInt
SMALLINT(%1)=%Library.SmallInt
SMALLMONEY=%Library.Currency
SYSNAME=%Library.String(MAXLEN=128)
TEXT=%Stream.GlobalCharacter
TIME=%Library.Time
TIME(%1)=%Library.Time(PRECISION=%1)
TIMESTAMP=%Library.PosixTime
TIMESTAMP2=%Library.TimeStamp
TINYINT=%Library.TinyInt
TINYINT(%1)=%Library.TinyInt
UNIQUEIDENTIFIER=%Library.UniqueIdentifier
VARBINARY=%Library.Binary(MAXLEN=1)
VARBINARY(%1)=%Library.Binary(MAXLEN=%1)
VARCHAR=%Library.String(MAXLEN=1)
VARCHAR(%1)=%Library.String(MAXLEN=%1)
VARCHAR(%1,%2)=%Library.String(MAXLEN=%1)
VARCHAR(MAX)=%Stream.GlobalCharacter
VARCHAR2(%1)=%Library.String(MAXLEN=%1)
VECTOR=%Library.Vector(DATATYPE="DOUBLE")
VECTOR(%1)=%Library.Vector(DATATYPE=%1)
VECTOR(%1,%2)=%Library.Vector(DATATYPE=%1,LEN=%2)

[Telnet]
DNSLookup=ON
Port=23

[Conversions]
LastConvertTime=2025-09-24 14:39:42

### 1.2 Editing the Active CPF

There are multiple ways to interact with the CPF, including through the Management Portal, API calls, or a text editor. For instructions on how to change a specific parameter , review the Changing This Parameter section of the reference page for that parameter. Some changes may require the instance to be restarted to take effect.

When using a text editor to modify the CPF, you must first shut do wn the instance. Open the iris.cpf file, located in the installation directory, and make the desired changes. InterSystems recommends that you save a backup copy of the CPF before editing it, as an invalid CPF may cause InterSystems IRIS to fail to start. Be sure to follow the syntax described in
CPF Format.

You can specify the CPF for InterSystems IRIS to use with the iris start command, or you can create a partial CPF to
merge into iris.cpf during deployment on UNIX® and Linux systems. These options are described in the following sections:

- Choosing a CPF at Startup

- Using the Configuration Mer ge Feature

#### 1.2.1 Choosing a CPF at Startup

If you frequently switch between two or more InterSystems IRIS configurations, such as for de velopment and testing purposes, you can create distinct CPFs for these purposes. When starting InterSystems IRIS, you can specify which .cpf file to use to reduce time spent manually changing settings.

For example, on Windows, if the InterSystems IRIS installation directory is C:\IRIS, you might have the following CPFs:

C:\IRIS\iris.cpf ; default CPF
C:\IRIS\production.cpf ; for production
C:\IRIS\development.cpf ; for development
C:\IRIS\testapps.cpf ; for testing
C:\IRIS\iris_customerbug.cpf ; for troubleshooting

To use a different CPF, you must first stop InterSystems IRIS. Then, start the instance with the iris start command, specifying the full path of the CPF InterSystems IRIS should use. The iris start command is described in the Controlling an InterSystems IRIS Instance.

At shutdown, the instance automatically saves the last known error-free configuration to a file called installation directory. You can use this file, if you need to, for reco very purposes.

_LastGood_.cpf in the

### 1.3 Using the Configuration Merge Feature

You can modify the default iris.cpf using a declarative configur ation merge file . A merge file is a partial CPF that sets the desired values for any number of CPF parameters.

Configuration mer ge is useful for a number of purposes. A merge file lets you specify indi vidual settings for instances deployed from the same source, supporting automated deployment and a DevOps approach. You can use configuration
merge with containerized and noncontainerized InterSystems IRIS instances; for more information, see Configuration
merge in deployment and Reconfigure an e xisting instance using configuration mer ge in Automating Configur ation of InterSystems IRIS with Configur ation Merge.

Configuration mer ge is very useful in automated deployment (on UNIX® and Linux systems only) because it makes the specified configuration changes before the ne w instance first starts, allo wing you to customize the configurations of multiple instances deployed from the same container image or install kit. Automated deployment of multinode topologies can use
multiple merge files to customize dif ferent groups of instances; for example, in automated deployment of a sharded cluster
with compute nodes, you would apply different merge files for data node 1, the remaining data nodes, and the compute nodes in that order, and when deploying a mirror, you would apply different merge files for the primary , backup, and async members.

Automated reconfiguration of multiple instances can be achie ved in the same way, by restarting groups of instances specifying the applicable merge file for each.

The iris merge command (available on Windows as well as UNIX and Linux systems) lets you use configuration mer ge to reconfigure a running instance. By automating application of the same mer ge file to multiple running instances using iris merge, you can simultaneously reconfigure all of those instances in the same w ay, applying the same set of configuration changes across your application or cluster. A single automated program can of course apply different merge files to dif ferent groups of instances (such as different mirror member or cluster nodes types) as described in the foregoing.

For an example of using a merge file, see the section that follo ws. For more information about using a merge file when deploying InterSystems IRIS containers, see Deploying an InterSystems IRIS Container with a Merge File. For information about using a merge file with the InterSystems K ubernetes Operator (IKO), see configSource: Create configuration files and a config map for them in Using the InterSystems Kubernetes Operator.

#### 1.3.1 Configuration Merge Example

This example describes how to use a merge file at startup to modify the shared memory heap and the database cache in a noncontainerized instance. These settings are controlled by the gmheap and globals parameters respectively.

The first step is to create the mer ge file. The example file belo w in named config_merge.cpf, though any name or extension is valid. Note that the merge file uses the same syntax as a CPF.

Configuration Security

# Example configuration merge file.

[config] globals=0,0,800,0,0,0 gmheap=256000

Next, use the iris stop command to shut down the target instance for the merge.

$ sudo iris stop IRIS

Finally, restart the instance with ISC_CPF_MERGE_FILE set, as in the following script.

When this script runs, InterSystems IRIS modifies the

iris.cpf file as specified in the

config_merge.cpf file.

#!/bin/bash

# Start InterSystems IRIS with the necessary parameters (all on one line).
sudo ISC_CPF_MERGE_FILE=/merge_files/config_merge.cpf iris start IRIS

When the instance starts up, the merge is complete! Check that the iris.cpf file contains the desired v alues for the gmheap and globals settings.

[config] ... errlog=500 globals=0,0,800,0,0,0 gmheap=256000 history=500,1024 ...

Note:

If the merge file specified by ISC_CPF_MERGE_FILE is not present, instance startup displays an error message and continues.

### 1.4 Configuration Security

To protect against accidental or intentional misconfiguration of the CPF , you can enable Configuration Security. This option is available on the System-wide Security Parameters page of the Management Portal (System Administration > Security > System Security > System-wide Security Parameters).

When Configuration Security is enabled, if InterSystems IRIS startup detects that the configuration parameter file has been modified since the last time InterSystems IRIS w as started, InterSystems IRIS startup requests a username and password to validate the changes. The user account supplied must have %Admin_Manage:Use privileges. If the appropriate username
and password cannot be provided, InterSystems IRIS allows the operator to choose as follows:

1. Re-enter the username and password.

2. Start using the last known good configuration.

3. Abort startup.

If the operator chooses option 2, InterSystems IRIS renames the parameter file that w as invoked at startup (file .cpf) with the suffix _rejected (file .cpf_rejected). InterSystems IRIS then overwrites the file .cpf with the last known good configuration (_LastGood_.cpf) and starts up using this configuration.

Note:

This Configuration Security setting is not a substitute for operating-system–level security. InterSystems recommends that you protect the configuration file by strictly limiting the ability of users to modify it, at the operating-system level.

For more information on other system-wide security parameters, see System Management and Security.

### 1.5 Parameter Descriptions

Each parameter reference page in this book includes most of the following sections:

- Synopsis – The CPF section that contains this parameter, followed by a summary of its syntax. Beneath this, a description of valid inputs and the default value.

- Description – A formal description of the parameter. May include examples of valid inputs or guidelines for choosing values.

- Changing this Parameter – The various ways to change this parameter, either programmatically or using the browserbased Management Portal.

- See Also – Links to related parameters and relevant documentation.

[Actions]

In addition to changing the values of configuration parameters, the configuration mer ge feature can create, modify, and delete dozens of different InterSystems IRIS objects, such as namespaces and databases, users, roles, and resource, and mirrors and mirror members, on both newly deployed and existing instances. This is done using the parameters in the [Actions] section, which is valid only in a merge file.

Important:

Do not add the [Actions] section directly to the Configuration P arameter File (CPF). The [Actions] section is not supported in the CPF, and causes instance startup to fail if included.

The operations specified in [Actions] are idempotent, meaning that the y are executed only if they would result in a change — if an object to be created exists, an object to be deleted does not exist, or an object to be modified matches the specified change, the operation is skipped. The order of the operations in the [Actions] section of a configuration mer ge file has no
effect on the order in which they are executed; InterSystems IRIS performs the operations in a deterministic order.

For complete information on the configuration mer ge feature, see Automating Configuration of InterSystems IRIS with
Configuration Mer ge; for an explanation of how the [Actions] parameters are used and lists of the parameters and details
about their usage, see Merge Actions and Useful Action Parameters in Automated Deployment, respectively, in that document.

[Actions]

CreateApplication

Create a new application.

Synopsis

[Actions]
CreateApplication:Name=ApplicationName[,Additional Properties]

Description
CreateApplication defines an application. InterSystems IRIS® creates the defined application when processing the [Actions] section during a configuration mer ge. You can specify any possible application properties in the CreateApplication definition. During the mer ge, InterSystems IRIS runs Security.Applications.Create() to create the application defined by CreateApplication. It only requires you to define Name and any additional properties needed
for that type of application; the complete list of application properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change CreateApplication by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Defining Applications for more information about managing applications from the management portal.

DeleteApplication

DeleteApplication

Delete an application.

Synopsis

[Actions]
DeleteApplication:ApplicationName

Description
DeleteApplication deletes an application from the security database. InterSystems IRIS® deletes the defined application when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Security.Applications.Delete() to delete the application defined by DeleteApplication. It only requires you to define Name.

This operation is designed to be used during a configuration mer ge. You can change DeleteApplication by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Defining Applications for more information about managing applications from the management portal.

[Actions]

ModifyApplication

Modify an application.

Synopsis

[Actions]
ModifyApplication:Name=ApplicationName,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyApplication modifies an e xisting application. InterSystems IRIS® modifies the defined application when processing the [Actions] section during a configuration mer ge. You specify which application properties you want to modify in the ModifyApplication definition. During the mer ge, InterSystems IRIS runs Security.Applications.Modify() to modify the application defined by ModifyApplication. It only requires you to define Name and the properties you
want to modify for that application; the complete list of application properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change ModifyApplication by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Defining Applications for more information about managing applications from the management portal.

Execute

Execute

Execute a class method or routine.

Synopsis

[Actions]
Execute:Namespace=namespace,[Additional Properties]

Description
Execute executes a class or routine. InterSystems IRIS® executes the defined class or routine when processing the [Actions] section during a configuration mer ge. The Execute parameters are always processed last. You specify the class or routine and its arguments in the Execute definition, and the class or routine must return a %Status (%Library.Status) value. During the merge, InterSystems IRIS runs the class or routine defined by Execute.

Examples

You can execute a class or a routine with Execute. For example:

Execute the class method SYS.ClassA.MethodZ() and pass three arguments to it; equivalent of
SYS.ClassA.MethodZ("arg1","arg2","arg3"):

Execute:Namespace="%SYS",ClassName="SYS.ClassA",MethodName="MethodZ",Arg1="arg1",Arg2="arg2",Arg3="arg3"

Load and compile classes from an XML file; equivalent of $SYSTEM.OBJ.Load("c:\iris\test.xml","ck"):

Execute:Namespace="%SYS",ClassName=%SYSTEM.OBJ,MethodName="Load",Arg1="c:\iris\test.xml",Arg2="ck"

Run a routine and pass two arguments to it; equivalent of $$Tag2^ZTEST("arg1","arg2"):

Execute:Namespace="%SYS",RoutineName="$$Tag2^ZTEST",Arg1="arg1",Arg2="arg2"

This operation is designed to be used during a configuration mer ge. You can change Execute by editing the merge file in a text editor (as described in Editing the Active CPF).

[Actions]

ConfigProduction

Configure a ne w production.

Synopsis

[Actions]
ConfigProduction:Namespace=Namespace,Path=ProductionFilePath,Name=ProductionName

Description
ConfigProduction deploys a production from an XML file, specifically an XML file created by e production. InterSystems IRIS® loads the defined production when processing the [Actions] section during a configuration merge. The action requires the namespace to run the production, the file path and filename to the XML file containing the production, and the production name.

xporting an existing

When using ConfigProduction, you can specify the optional AutoStart (1 or 0) property. The default value of AutoStart
is 0; when set to 1, the production is configured as an Auto-Start Production.

When configuring a namespace with the ConfigProduction action:

- If the specified namespace does not e xist, InterSystems IRIS automatically performs a few operations. InterSystems IRIS creates a new database resource that protects two new databases: a default database for globals and a default database for routines. The name of the resource and each database is based on the value passed to Namespace defined in ConfigProduction, the databases are created in the install-dir/mgr directory. InterSystems IRIS will then create the namespace, using the newly created databases, and enable it for interoperability.

- If the specified namespace already e xists but lacks interoperability support, InterSystems IRIS will automatically enable interoperability features for the namespace.

This operation is designed to be used during a configuration mer ge. You can change ConfigProduction by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Defining Applications for more information about managing applications from the management portal.

CreateComPort

CreateComPort

Create a COM port definition in the [ComPorts] section of the CPF .

Synopsis

[Actions]
CreateComPort:Name=ComPortName[,Additional Properties]

Description
CreateComPort defines a COM port in the [ComPorts] section of the CPF . InterSystems IRIS® creates the defined COM port when processing the [Actions] section during a configuration mer ge. You can specify any possible COM port properties in the CreateComPort definition. During the mer ge, InterSystems IRIS runs Config.ComPorts.Create() to create the COM port defined by CreateComPort. It only requires you to define Name. The complete list of COM port properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change CreateComPort by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Terminal I/O for more information about COM ports.

[Actions]

DeleteComPort

Delete a COM port definition in the [ComPorts] section of the CPF .

Synopsis

[Actions]
DeleteComPort:Name=ComPortName

Description
DeleteComPort deletes a defined COM port in the [ComPorts] section of the CPF . InterSystems IRIS® deletes the defined COM port when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.ComPorts.Delete() to delete the COM port. It only requires you to define the Name parameter to identify the COM port.

This operation is designed to be used during a configuration mer ge. You can change DeleteComPort by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Terminal I/O for more information about COM ports.

ModifyComPort

ModifyComPort

Modify a COM port definition in the [ComPorts] section of the CPF .

Synopsis

[Actions]
ModifyComPort:Name=ComPortName,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyComPort modifies a COM port defined in the [ComPorts] section of the CPF . InterSystems IRIS® modifies the defined COM port when processing the [Actions] section during a configuration mer ge. You specify which COM port properties you want to modify in the ModifyComPort definition. During the mer ge, InterSystems IRIS runs Config.Com- Ports.Modify() to modify the COM port. The complete list of COM port properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change ModifyComPort by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Terminal I/O for more information about COM ports.

[Actions]

ModifyConfig

Modify the CPF [config] parameters.

Synopsis

[Actions]
ModifyConfig:[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyConfig modifies the [config] parameters in the CPF . InterSystems IRIS® modifies the defined properties when processing the [Actions] section during a configuration mer ge. You specify which properties you want to modify in the ModifyConfig definition. During the mer ge, InterSystems IRIS runs Config.config.Modify() to modify the properties. The complete list of [config] properties is a vailable in the class reference.

This operation is designed to be used during a configuration mer ge. You can change ModifyConfig by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See [config] for more details about each of the properties you can modify.

CreateDatabase

CreateDatabase

Create a new database.

Synopsis

[Actions] CreateDatabase:Name=DatabaseName,Directory=DatabaseDirectory[,Additional Properties]

Description
CreateDatabase defines a database. InterSystems IRIS® creates that database when processing the [Actions] section during a configuration mer ge.

You can specify any possible database properties in the CreateDatabase definition. During the mer ge, InterSystems IRIS runs SYS.Database.CreateDatabase() and Config.Databases.Create() to create the database defined by
CreateDatabase. Only the Name and Directory properties are required; the complete list of properties is available in
the class reference.

When using CreateDatabase on an ECP client to add a remote database, there are two unique required properties. These
are:

- Server – This specifies the name of the ECP serv er where the remote database is located.

- LogicalOnly (1 or 0) – When set to 1, this prevents CreateDatabase from creating a physical database on the ECP client. This suppresses the call of SYS.Database.CreateDatabase() during the merge.

When using CreateDatabase on a mirror member to add a mirrored database, the MirrorSetName and MirrorDBName
properties are required for a mirrored database, there are also the following optional properties:

- LogicalOnly (1 or 0) – When set to 1, and during the merge on the current primary, this adds a preexisting database to the mirror defined in MirrorSetName by modifying it using the SYS.Mirror.AddDatabase() method. During the merge on a nonprimary mirror member, the LogicalOnly property is ignored and a new empty physical database is created to receive download from the primary mirror member using the automatic database download feature. When set to 0, this creates a new mirrored database.

If the preexisting database exists on all mirror members at the same location, specified in CreateDatabase, creation of this new or empty mirrored database will fail.

- Seed – This specifies a file path to an e xisting database.

During the merge on the primary mirror member and if Seed is specified, InterSystems IRIS copies the data from the existing database into the new mirrored database defined by CreateDatabase. This converts a preexisting nonmirrored database to a mirrored database on the primary mirror member. When adding a preexisting database to a mirror, the copy operation will only be carried out on the current primary mirror member. This property can be used copy an existing database to a new location before adding it to a mirror to avoid failure of creating the new database discussed above. During the merge on a nonprimary mirror member Seed is ignored.

Seed may also be provided for a nonmirrored database to copy the data from the existing database into the new nonmirrored database.

The section below contains an example of using these properties to add a remote database and to add a mirrored database using the Seed property.

Example

Upon running a configuration mer ge, the following example creates the Customers database, specifying the initial size and
maximum size:

[Actions]

[Actions]
CreateDatabase:Name=Customers,Directory=/IRIS/mgr/Customers,Size=5368,MaxSize=536871

If the Customers database is a remote database located on Node1, the [Actions] section would instead look like:

[Actions]
CreateDatabase:Name=Customers,Directory=/IRIS/mgr/Customers,Server=Node1,LogicalOnly=1

Upon running a configuration mer ge, the following example creates the mirorred Customers database, specifying the
existing database to copy:

[Actions]
CreateDatabase:Name=Customers,Directory=/IRIS/mgr/Customers,MirrorSetName=CUSTOMERSMIRROR,MirrorDBName=Customers,Seed=/mnt/databases/ExistingDB

This operation is designed to be used during a configuration mer ge. You can change CreateDatabase by editing the merge file in a te xt editor (as described in Editing the Active CPF).

For information about creating and editing a database from the Management Portal, see Configuring Databases .

DeleteDatabase

DeleteDatabase

Delete an existing database.

Synopsis

[Actions] DeleteDatabase:Name=DatabaseName,Directory=DatabaseDirectory

Description
DeleteDatabase specifies a database to delete. InterSystems IRIS® deletes that database when processing the [Actions] section during a configuration mer ge.

Specify the Name and the Directory of the database to delete. During the merge, InterSystems IRIS runs SYS.Database.DeleteDatabase() to delete the database specified by DeleteDatabase.

This operation is designed to be used during a configuration mer ge. You can change DeleteDatabase by editing the merge file in a te xt editor (as described in Editing the Active CPF).

For information about deleting a database from the Management Portal, see Configuring Databases .

[Actions]

ModifyDatabase

Modify an existing database.

Synopsis

[Actions] ModifyDatabase:Name=DatabaseName,Directory=DatabaseDirectory[,Additional Properties]

Where you substitute [,Additional Properties] for the additional properties you want to modify. ModifyDatabase can
modify logical database properties and the physical database properties listed below:

- Size

- MaxSize

- ExpansionSize

- ResourceName

- GlobalJournalState

- NewGlobalIsKeep

- NewGlobalCollation

- ReadOnly

Description
ModifyDatabase specifies changes to mak e to a database. InterSystems IRIS® modifies the specified database when processing the [Actions] section during a configuration mer ge.

You can specify physical and logical database properties in the ModifyDatabase definition. During the mer ge, InterSystems IRIS runs an API call and then runs Config.Databases.Modify() to modify the database as specified by ModifyDatabase. The complete list of database properties is available in the class reference.

If you want to only modify logical database properties with ModifyDatabase, you can supply the optional LogicalOnly (1 or 0) property. When set to 1, this suppresses the API call that modifies ph ysical database properties.

This operation is designed to be used during a configuration mer ge. You can change ModifyDatabase by editing the merge file in a te xt editor (as described in Editing the Active CPF).

For information about modifying a database from the Management Portal, see Configuring Databases .

CreateDatabaseFile

CreateDatabaseFile

Create a database file. Does not re gister the database in InterSystems IRIS®.

Synopsis

[Actions]
CreateDatabaseFile:Directory=DatabaseDirectory[,Additional Properties]

Description
CreateDatabaseFile specifies the location on the host file system where InterSystems IRIS® creates a database file. InterSystems IRIS does not register this database in the instance. InterSystems IRIS creates the database file at the specified location when processing the [Actions] section during a configuration mer ge. You can specify additional properties for the database file. The complete list of database properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change CreateDatabaseFile by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Configuring Databases for information about databases, .

[Actions]

DeleteDatabaseFile

Delete a database file.

Synopsis

[Actions]
DeleteDatabaseFile:Directory=DatabaseDirectory

Description
DeleteDatabaseFile specifies the location on the host file system where InterSystems IRIS® deletes a database file. InterSystems IRIS deletes the database file at the specified location when processing the uration merge.

[Actions] section during a config-

This operation is designed to be used during a configuration mer ge. You can change DeleteDatabaseFile by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Configuring Databases for information about databases, .

ModifyDatabaseFile

ModifyDatabaseFile

Modify a database file.

Synopsis

[Actions]
ModifyDatabaseFile:Directory=DatabaseDirectory,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify. The properties that
ModifyDatabaseFile can modify are the following:

- Size

- MaxSize

- ExpansionSize

- ResourceName

- GlobalJournalState

- NewGlobalIsKeep

- NewGlobalCollation

- ReadOnly

Description
ModifyDatabaseFile specifies the location on the host file system where InterSystems IRIS® modifies a database file. InterSystems IRIS modifies the database file at the specified location according to the defined properties when processing the [Actions] section during a configuration mer ge. The complete list of database properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change ModifyDatabaseFile by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Configuring Databases for information about databases, .

[Actions]

CreateDevice

Create a device.

Synopsis

[Action]
CreateDevice:Name=DeviceName,PhysicalDevice=PhysicalDeviceName,SubType=DeviceSubtypes,Type=x[,Additional
Properties]

Where the value of x is one of the following options:

- TRM — Terminal

- SPL — Spooling device

- MT — Magnetic tape drive

- BT — Cartridge tape drive

- IPC — Interprocess communication

- OTH — Any other device including printers and sequential files

Description
CreateDevice defines a de vice. InterSystems IRIS® creates that device when processing the [Actions] section during a configuration mer ge. You can specify any possible device properties in the CreateDevice definition. During the mer ge, InterSystems IRIS runs Config.De vices.Create() to create the device defined by CreateDevice. Only the Name,
PhysicalDevice, SubType, and Type properties are required; the complete list of properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change CreateDevice by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Introduction to I/O for more details on devices.

DeleteDevice

DeleteDevice

Delete a device.

Synopsis

[Actions] DeleteDevice:Name=DeviceName

Description
DeleteDevice deletes an existing device. InterSystems IRIS® deletes the specified de vice when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.De vices.Delete() to delete the device defined by DeleteDevice.

This operation is designed to be used during a configuration mer ge. You can change DeleteDevice by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Introduction to I/O for more details on devices.

[Actions]

ModifyDevice

Modify a device.

Synopsis

[Actions]
ModifyDevice:Name=DeviceName,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyDevice specifies changes to mak e to an existing device. InterSystems IRIS® modifies this de vice when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.De vices.Modify() to modify the device defined by ModifyDevice.

This operation is designed to be used during a configuration mer ge. You can change ModifyDevice by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Introduction to I/O for more details on devices.

CreateDeviceSubType

CreateDeviceSubType

Create a device subtype.

Synopsis

[Actions] CreateDeviceSubType:Name=DeviceSubTypeName,ScreenLength=n[,Additional Properties]

Where n is the number of lines that comprise one screen or page for the device. Default is zero.

Description
CreateDeviceSubType defines a de vice subtype. InterSystems IRIS® creates that device subtype when processing the [Actions] section during a configuration mer ge. You can specify any possible device subtype properties in the CreateDeviceSubType definition. During the mer ge, InterSystems IRIS runs Config.De viceSubTypes.Create() to create
the device subtype defined by CreateDeviceSubType. Only the Name and ScreenLength properties are required; the
complete list of properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change CreateDeviceSubType by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Introduction to I/O for more details on devices.

[Actions]

DeleteDeviceSubType

Delete a device subtype.

Synopsis

[Actions]
DeleteDeviceSubType:Name=DeviceSubTypeName

Description
DeleteDeviceSubType deletes an existing device subtype. InterSystems IRIS® deletes the specified de vice subtype when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.De viceSubTypes.Delete() to delete the device subtype defined by DeleteDeviceSubType.

This operation is designed to be used during a configuration mer ge. You can change DeleteDeviceSubType by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Introduction to I/O for more details on devices.

ModifyDeviceSubType

ModifyDeviceSubType

Modify a device subtype.

Synopsis

[Actions]
ModifyDeviceSubType:Name=DeviceSubTypeName,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyDeviceSubType specifies changes to mak e to an existing device subtype. InterSystems IRIS® modifies this device subtype when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.De viceSubTypes.Modify() to modify the device subtype defined by ModifyDeviceSubType.

This operation is designed to be used during a configuration mer ge. You can change ModifyDeviceSubType by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Introduction to I/O for more details on devices.

[Actions]

CreateDocDB

Create a document database (DocDB) application definition.

Synopsis

[Actions]
CreateDocDB:Name=DocDBName,Namespace=Namespace[,Additional Properties]

Description
CreateDocDB defines a document database application definition. InterSystems IRIS® creates that document database application definition when processing the [Actions] section during a configuration mer ge. You can specify any possible property for a document database application definition in the CreateDocDB definition. During the mer ge, InterSystems IRIS runs Security.DocDBs.Create() to create the document database application definition defined by CreateDocDB.
Only the Name and Namespace properties are required; the complete list of properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change CreateDocDB by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Document Database Applications for more details on document database applications.

See Introduction to Document Database for more details on document databases.

DeleteDocDB

DeleteDocDB

Delete a document database (DocDB) application definition.

Synopsis

[Actions]
DeleteDocDB:Name=DocDBName,Namespace=Namespace

Description
DeleteDocDB deletes an existing document database application definition. InterSystems IRIS® deletes the specified document database application definition when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Security.DocDBs.Delete() to delete the document database application definition defined by DeleteDocDB.

This operation is designed to be used during a configuration mer ge. You can change DeleteDocDB by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Document Database Applications for more details on document database applications.

See Introduction to Document Database for more details on document databases.

[Actions]

ModifyDocDB

Modify a document database (DocDB) application definition.

Synopsis

[Actions]
ModifyDocDB:Name=DocDBName,Namespace=Namespace,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyDocDB specifies changes to mak e to an existing document database application definition. InterSystems IRIS® [Actions] section during a configuration mer ge. modifies this document database application definition when processing the During the merge, InterSystems IRIS runs Security.DocDBs.Modify() to modify the document database application definition defined by ModifyDocDB.

This operation is designed to be used during a configuration mer ge. You can change ModifyDocDB by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Document Database Applications for more details on document database applications.

See Introduction to Document Database for more details on document databases.

ModifyECP

ModifyECP

Modify an ECP configuration.

Synopsis

[Actions]
ModifyECP:Name=ECPConfigName,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyECP specifies changes to mak e to an existing ECP configuration. InterSystems IRIS® modifies this ECP configuration when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.ECP .Modify() to modify the ECP configuration defined by ModifyECP.

This operation is designed to be used during a configuration mer ge. You can change ModifyECP by editing the merge file in a text editor (as described in Editing the Active CPF).

See Deploying ECP for more details on ECP configurations.

[Actions]

CreateECPServer

Create an ECP server.

Synopsis

[Actions]
CreateECPServer:Name=ECPServerName,Address=Hostname,MirrorConnection=x,Port=n,SSLConfig=z

Where x is one of the following three values:

- 0 — Non-mirrored connection. Default.

- 1 — Mirrored connection.

- -1 — Mirrored connection restricted to the configured mirror member only .

Where n is the port number to connect to. Defaults to the superserver port.

Where z is either 0 for do not use the TLS configuration (%ECPClient) or 1 for do use the TLS configuration (%ECPClient). Default is 0.

Description
CreateECPServer defines an ECP serv er. InterSystems IRIS® creates that ECP server when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.ECPServ ers.Create() to create the
ECP server defined by CreateECPServer. All properties are required; the complete list of properties is available in the
class reference.

This operation is designed to be used during a configuration mer ge. You can change CreateECPServer by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Deploying ECP for more details on ECP configurations.

DeleteECPServer

DeleteECPServer

Delete an ECP server.

Synopsis

[Actions]
DeleteECPServer:Name=ECPServerName

Description
DeleteECPServer deletes an existing ECP server. InterSystems IRIS® deletes the specified ECP serv er when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.ECPServ ers.Delete() to delete the ECP server defined by DeleteECPServer.

This operation is designed to be used during a configuration mer ge. You can change DeleteECPServer by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Deploying ECP for more details on ECP configurations.

[Actions]

ModifyECPServer

Modify an ECP server.

Synopsis

[Actions]
ModifyECPServer:Name=ECPServerName,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyECPServer specifies changes to mak e to an existing ECP server. InterSystems IRIS® modifies this ECP serv er when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.ECPServ ers.Modify() to modify the ECP server defined by ModifyECPServer.

This operation is designed to be used during a configuration mer ge. You can change ModifyECPServer by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Deploying ECP for more details on ECP configurations.

CreateEvent

CreateEvent

Create a new audit event.

Synopsis

[Actions] CreateEvent:Source=EventSource,Type=EventType,Name=EventName,Enabled=x[,Additional Properties]

Where x is 1 for enabled or 0 for disabled.

Description
CreateEvent specifies an audit e vent to create. InterSystems IRIS® creates that audit event when processing the [Actions] section during a configuration mer ge.

You can specify any possible audit event properties in the CreateEvent definition. During the mer ge, InterSystems IRIS runs Security.Events.Create() to create the audit event defined by CreateEvent.

This operation is designed to be used during a configuration mer ge. You can change CreateEvent by editing the merge file in a te xt editor (as described in Editing the Active CPF).

For information about creating an audit event from the Management Portal, see Auditing.

[Actions]

DeleteEvent

Delete an audit event.

Synopsis

[Actions] DeleteEvent:Source=EventSource,Type=EventType,Name=EventName

Description
DeleteEvent deletes an existing audit event. InterSystems IRIS® deletes the specified audit e vent when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Security.Events.Delete() to delete the audit event defined by DeleteEvent.

This operation is designed to be used during a configuration mer ge. You can change DeleteEvent by editing the merge file in a te xt editor (as described in Editing the Active CPF).

For information about audit events, see Auditing.

ModifyEvent

ModifyEvent

Modify an audit event.

Synopsis

[Actions] ModifyEvent:Source=EventSource,Type=EventType,Name=EventName,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyEvent specifies changes to mak e to an existing audit event. InterSystems IRIS® modifies this audit e vent when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Security.Events.Modify() to modify the audit event defined by ModifyEvent.

This operation is designed to be used during a configuration mer ge. You can change ModifyEvent by editing the merge file in a te xt editor (as described in Editing the Active CPF).

For information about audit events, see Auditing.

[Actions]

ModifyJournal

Modify the [Journal] CPF settings.

Synopsis

[Actions]
ModifyJournal:[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyJournal specifies changes to mak e to the journal settings. InterSystems IRIS® modifies the journal settings when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.Jour - nal.Modify() to modify the journal settings defined by ModifyJournal.

This operation is designed to be used during a configuration mer ge. You can change ModifyJournal by editing the merge file in a te xt editor (as described in Editing the Active CPF).

For information about journaling, see Journaling.

CreateLDAPConfig

CreateLDAPConfig

Create an LDAP configuration.

Synopsis

[Actions]
CreateLDAPConfig:Name=LDAPConfigName[,Additional Properties]

Description
CreateLDAPConfig defines an LD AP configuration. InterSystems IRIS® creates that LD AP configuration when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Security.LDAPCon-
figs.Create() to create the LD AP configuration defined by CreateLDAPConfig. Many properties are required; the complete
list of properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change CreateLDAPConfig by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See LDAP Configuration Fields for more information about LDAP configurations.

[Actions]

DeleteLDAPConfig

Delete an LDAP configuration.

Synopsis

[Actions]
DeleteLDAPConfig:Name=LDAPConfigName

Description
DeleteLDAPConfig deletes an existing LDAP configuration. InterSystems IRIS® deletes the specified LD AP configuration when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Security.LDAPConfigs.Delete() to delete the LD AP configuration defined by DeleteLDAPConfig.

This operation is designed to be used during a configuration mer ge. You can change DeleteLDAPConfig by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See LDAP Configuration Fields for more information about LDAP configurations.

ModifyLDAPConfig

ModifyLDAPConfig

Modify an LDAP configuration.

Synopsis

[Actions]
ModifyLDAPConfig:Name=LDAPConfigName,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyLDAPConfig specifies changes to mak e to an existing LDAP configuration. InterSystems IRIS® modifies the LDAP configuration when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Security.LDAPConfigs.Modify() to modify the LD AP configuration defined by ModifyLDAPConfig.

This operation is designed to be used during a configuration mer ge. You can change ModifyLDAPConfig by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See LDAP Configuration Fields for more information about LDAP configurations.

[Actions]

CreateLicenseServer

Create a license server.

Synopsis

[Actions]
CreateLicenseServer:Name=LicenseServerName,Address=LicenseServerAddress,Port=n[,Additional Properties]

Description
CreateLicenseServer defines a license serv er. InterSystems IRIS® creates that license server when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.LicenseServ ers.Create()
to create the license server defined by CreateLicenseServer. The Name, Address and Port properties are required;
the complete list of properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change CreateLicenseServer by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Configuring a License Serv er for more information about license servers.

DeleteLicenseServer

DeleteLicenseServer

Delete a license server.

Synopsis

[Actions]
DeleteLicenseServer:Name=LicenseServerName

Description
DeleteLicenseServer deletes an existing license server. InterSystems IRIS® deletes the specified license serv er when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.LicenseServers.Delete() to delete the license server defined by DeleteLicenseServer.

This operation is designed to be used during a configuration mer ge. You can change DeleteLicenseServer by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Configuring a License Serv er for more information about license servers.

[Actions]

ModifyLicenseServer

Modify a license server.

Synopsis

[Actions]
ModifyLicenseServer:Name=LicenseServerName,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyLicenseServer specifies changes to mak e to an existing license server. InterSystems IRIS® modifies the license server when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.LicenseServ ers.Modify() to modify the license server defined by ModifyLicenseServer.

This operation is designed to be used during a configuration mer ge. You can change ModifyLicenseServer by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Configuring a License Serv er for more information about license servers.

ImportLocale

ImportLocale

Import a custom locale.

Synopsis

[Actions]
ImportLocale:FileName=Filename,Flags=Flag

Where you substitute Filename with the full file path and file name of the desired XML file.

Where you substitute Flag with 1 to load only subtables, with 2 to load only tables, with 4 to load the locale only, and 7 to load subtables, tables, and the locale.

Description
ImportLocale imports a custom locale from an XML file. InterSystems IRIS® imports the locale when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.NLS.Locales.ImportAll() to import the locale defined by ImportLocale.

This operation is designed to be used during a configuration mer ge. You can change ImportLocale by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Configuring National Language Support (NLS) for more information about configuring national language settings.

See Using System Classes for National Language Support for more information about creating custom locales and the appropriate naming convention.

[Actions]

InstallLocale

Install a locale as the default locale.

Synopsis

[Actions]
InstallLocale:Name=LocaleName

Where you substitute LocaleName with the name of the desired locale.

Description
InstallLocale installs a locale as the default locale for the system; the locale can be one of the system defined locales
or a custom locale which was imported with ImportLocale. InterSystems IRIS® installs the locale when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.NLS.Locales.Install() to install the locale defined by InstallLocale.

This operation is designed to be used during a configuration mer ge. You can change InstallLocale by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Configuring National Language Support (NLS) for more information about configuring national language settings.

ModifyLogging

ModifyLogging

Modify the [Logging] section of the CPF.

Synopsis

[Actions]
ModifyLogging:[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyLogging specifies changes to mak e to the [Logging] section of the CPF. InterSystems IRIS® modifies the [Logging] section of the CPF when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.Logging.Modify() to modify the [Logging] section of the CPF defined by ModifyLogging. The complete list of [Logging] properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change ModifyLogging by editing the merge file in a te xt editor (as described in Editing the Active CPF).

[Actions]

CreateMagTape

Create a magnetic tape definition in the [MagT apes] section of the CPF.

Synopsis

[Actions]
CreateMagTape:Name=MagTapeName,SystemDevice=TapeDeviceName

Description
CreateMagTape defines a magnetic tape. InterSystems IRIS® creates that magnetic tape definition when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.MagT apes.Create() to
create the magnetic tape definition defined by CreateMagTape. The Name and SystemDevice properties are required; the
complete list of properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change CreateMagTape by editing the merge file in a te xt editor (as described in Editing the Active CPF).

DeleteMagTape

DeleteMagTape

Delete a magnetic tape definition in the [MagT apes] section of the CPF.

Synopsis

[Actions]
DeleteMagTape:Name=MagTapeName

Description
DeleteMagTape deletes an existing magnetic tape definition. InterSystems IRIS® deletes the specified magnetic tape definition when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.MagT apes.Delete() to delete the magnetic tape definition defined by DeleteMagTape.

This operation is designed to be used during a configuration mer ge. You can change DeleteMagTape by editing the merge file in a te xt editor (as described in Editing the Active CPF).

[Actions]

ModifyMagTape

Modify a magnetic tape definition in the [MagT apes] section of the CPF.

Synopsis

[Actions]
ModifyMagTape:Name=MagTapeName,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyMagTape specifies changes to mak e to an existing magnetic tape definition. InterSystems IRIS® modifies the magnetic tape definition when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.MagT apes.Modify() to modify the magnetic tape definition defined by ModifyMagTape.

This operation is designed to be used during a configuration mer ge. You can change ModifyMagTape by editing the merge file in a te xt editor (as described in Editing the Active CPF).

CreateMapGlobal

CreateMapGlobal

Create a new global mapping.

Synopsis

[Actions] CreateMapGlobal:Namespace=Namespace,Name=MappingName,Database=Database[,Additional Properties]

Description
CreateMapGlobal specifies a global mapping to create. InterSystems IRIS® creates that global mapping when processing the [Actions] section during a configuration mer ge.

You can specify any possible global mapping properties in the CreateMapGlobal definition. During the mer ge, InterSystems IRIS runs Config.MapGlobals.Create() to create the global mapping defined by CreateMapGlobal.

This operation is designed to be used during a configuration mer ge. You can change CreateMapGlobal by editing the merge file in a te xt editor (as described in Editing the Active CPF).

For information about creating a global mapping from the Management Portal, see Global Mappings.

[Actions]

DeleteMapGlobal

Delete an existing global mapping.

Synopsis

[Actions] DeleteMapGlobal:Namespace=Namespace,Name=MappingName,Database=Database

Description
DeleteMapGlobal specifies a global mapping to delete. InterSystems IRIS® deletes the specified global mapping when processing the [Actions] section during a configuration mer ge.

Specify the Namespace, Name, and Database for the global mapping to delete. During the merge, InterSystems IRIS runs Config.MapGlobals.Delete() to delete the global mapping defined by DeleteMapGlobal.

This operation is designed to be used during a configuration mer ge. You can change DeleteMapGlobal by editing the merge file in a te xt editor (as described in Editing the Active CPF).

ModifyMapGlobal

ModifyMapGlobal

Modify an existing global mapping.

Synopsis

[Actions] ModifyMapGlobal:Namespace=Namespace,Name=MappingName,Database=Database[,Additional Properties]

Description
ModifyMapGlobal specifies changes to mak e to a global mapping. InterSystems IRIS® modifies the specified global mapping when processing the [Actions] section during a configuration mer ge.

You can specify any possible global mapping properties in the ModifyMapGlobal definition. During the mer ge, InterSystems IRIS runs Config.MapGlobals.Modify() to mak e the changes defined by ModifyMapGlobal.

This operation is designed to be used during a configuration mer ge. You can change ModifyMapGlobal by editing the merge file in a te xt editor (as described in Editing the Active CPF).

[Actions]

CreateMapPackage

Create a package mapping.

Synopsis

[Actions]
CreateMapPackage:Namespace=NSToMap,Name=MappingName,Database=DBToMap

Description
CreateMapPackage defines a package mapping. InterSystems IRIS® creates that package mapping when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.MapP ackages.Create() to create the package mapping defined by CreateMapPackage. The Namespace, Name, and Database properties are
required; the complete list of properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change CreateMapPackage by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Adding Mappings to a Namespace for more information on mappings.

DeleteMapPackage

DeleteMapPackage

Delete a package mapping.

Synopsis

[Actions]
DeleteMapPackage:Namespace=NSToMap,Name=MappingName

Description
DeleteMapPackage deletes an existing package mapping. InterSystems IRIS® deletes the specified package mapping when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.Map- Packages.Delete() to delete the package mapping defined by DeleteMapPackage.

This operation is designed to be used during a configuration mer ge. You can change DeleteMapPackage by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Adding Mappings to a Namespace for more information on mappings.

[Actions]

ModifyMapPackage

Modify a package mapping.

Synopsis

[Actions]
ModifyMapPackage:Namespace=NSToMap,Name=MappingName,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyMapPackage specifies changes to mak e to an existing package mapping. InterSystems IRIS® modifies the package mapping when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.MapP ackages.Modify() to modify the package mapping defined by ModifyMapPackage.

This operation is designed to be used during a configuration mer ge. You can change ModifyMapPackage by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Adding Mappings to a Namespace for more information on mappings.

CreateMapRoutine

CreateMapRoutine

Create a new routine mapping.

Synopsis

[Actions] CreateMapRoutine:Namespace=Namespace,Name=MappingName,Database=Database[,Additional
Properties]

Description
CreateMapRoutine specifies a routine mapping to create. InterSystems IRIS® creates that routine mapping when processing the [Actions] section during a configuration mer ge.

You can specify any possible routine mapping properties in the CreateMapRoutine definition. During the mer ge, InterSystems IRIS runs Config.MapRoutines.Create() to create the routine mapping defined by CreateMapRoutine.

This operation is designed to be used during a configuration mer ge. You can change CreateMapRoutine by editing the merge file in a te xt editor (as described in Editing the Active CPF).

For information about creating a routine mapping from the Management Portal, see Routine Mappings.

[Actions]

DeleteMapRoutine

Delete an existing routine mapping.

Synopsis

[Actions] DeleteMapRoutine:Namespace=Namespace,Name=MappingName,Database=Database[,Additional
Properties]

Description
DeleteMapRoutine specifies a routine mapping to delete. InterSystems IRIS® deletes the specified routine mapping when processing the [Actions] section during a configuration mer ge.

Specify the Namespace, Name, and Database for the routine mapping to delete. During the merge, InterSystems IRIS runs Config.MapRoutines.Delete() to delete the routine mapping defined by DeleteMapRoutine.

This operation is designed to be used during a configuration mer ge. You can change DeleteMapRoutine by editing the merge file in a te xt editor (as described in Editing the Active CPF).

ModifyMapRoutine

ModifyMapRoutine

Modify an existing routine mapping.

Synopsis

[Actions] ModifyMapRoutine:Namespace=Namespace,Name=MappingName,Database=Database[,Additional
Properties]

Description
ModifyMapRoutine specifies changes to mak e to a routine mapping. InterSystems IRIS® modifies the specified routine mapping when processing the [Actions] section during a configuration mer ge.

You can specify any possible routine mapping properties in the ModifyMapRoutine definition. During the mer ge, InterSystems IRIS runs Config.MapRoutines.Modify() to mak e the changes defined by ModifyMapRoutine.

This operation is designed to be used during a configuration mer ge. You can change ModifyMapRoutine by editing the merge file in a te xt editor (as described in Editing the Active CPF).

[Actions]

ConfigMirror

Configure a mirrored deplo yment.

Synopsis

[Actions]
ConfigMirror:Name=Name,Member=Member,Primary=Primary[,Additional Properties]

Where:

- Name is the name of the new mirror (when deploying a primary) or the name of the mirror to join (when deploying a backup, DR async, or reporting async).

- Member is the mirror member type, primary, backup, drasync, rorasync, rwrasync, or auto.

- Primary is the IP address or name of the primary’s host.

The above properties are required, however, there are many possible optional arguments you can supply to ConfigMirror, some that are commonly used are described below.

- SystemName is the name of the mirror member. If this property is not specified, the mirror member name is automatically derived from the hostname.

- SSLDir is the location on the host of the mirror TLS/SSL configuration for the instance, a directory containing the required Certificate Authority certificate ( CAFile.pem), local certificate ( CertificateFile.pem), and private key file (PrivateKeyFile.pem).

- ArbiterURL is the host (hostname or IP address) and port of the arbiter to be configured for the mirror (when deplo ying the primary) or configured for e xisting primary (when deploying a backup, DR async, or reporting async).

If you are using ConfigMirror to deploy a mirror using a single merge file and using hostname matching , the following arguments are needed.

- The Member property should be set to auto to automatically match mirror members to hostnames.

- The Primary property should be set to auto to automatically match mirror members to hostnames.

- Map is optional and sets the pattern used to match mirror members with hostnames; the default is Map="primary,backup"

- Ordinal is optional and an integer offset used to prevent overlap in hostnames between independent clusters; default
is 0 (zero).

Description
ConfigMirror defines a mirror configuration. InterSystems IRIS® deplo the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs a series of API calls to deploy the mirror configuration.

ys that mirror configuration when processing

ConfigMirror is reentrant and will exit if the mirror already exists without updating any properties. The exception is the property ArbiterURL which allows for the addition of an arbiter if one was not previously configured.

In addition to the properties described above, you can specify any property in the inventory of the MirrorInfo parameter of the SYS.Mirror.CreateNewMirrorSet() method.

For examples on using ConfigMirror, see Mirror the Cluster’s Data Server.

This operation is designed to be used during a configuration mer ge. You can change ConfigMirror by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Configuring Mirroring for more information about mirror configurations.

ConfigMirror

[Actions]

CreateMirror

Create a mirror configuration.

Synopsis

[Actions]
CreateMirror:Name=MirrorName[,Additional Properties]

Description
CreateMirror defines a mirror configuration. InterSystems IRIS® creates that mirror configuration when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.Mirrors.Create() to
create the mirror configuration defined by CreateMirror. The Name property is required; the complete list of properties
is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change CreateMirror by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Configuring Mirroring for more information about mirror configurations.

DeleteMirror

DeleteMirror

Delete a mirror configuration.

Synopsis

[Actions]
DeleteMirror:Name=MirrorName

Description
DeleteMirror deletes an existing mirror configuration. InterSystems IRIS® deletes the specified mirror configuration when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.Mir - rors.Delete() to delete the mirror configuration defined by DeleteMirror.

This operation is designed to be used during a configuration mer ge. You can change DeleteMirror by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Configuring Mirroring for more information about mirror configurations.

[Actions]

ModifyMirror

Modify a mirror configuration.

Synopsis

[Actions]
ModifyMirror:Name=MirrorName,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyMirror specifies changes to mak e to an existing mirror configuration. InterSystems IRIS® modifies the mirror configuration when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.Mirrors.Modify() to modify the mirror configuration defined by

ModifyMirror.

This operation is designed to be used during a configuration mer ge. You can change ModifyMirror by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Configuring Mirroring for more information about mirror configurations.

ModifyMirrorMember

ModifyMirrorMember

Modify a mirror member.

Synopsis

[Actions]
ModifyMirrorMember:Name=MirrorMemberName,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyMirrorMember specifies changes to mak e to an existing mirror member configuration. InterSystems IRIS® modifies the mirror member configuration when processing the the merge, InterSystems IRIS runs Config.MirrorMember .Modify() to modify the mirror member configuration defined by ModifyMirrorMember.

[Actions] section during a configuration mer ge. During

This operation is designed to be used during a configuration mer ge. You can change ModifyMirrorMember by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Configuring Mirroring for more information about mirror configurations.

[Actions]

ModifyMiscellaneous

Modify the [Miscellaneous] section of the CPF.

Synopsis

[Actions]
ModifyMiscellaneous:[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyMiscellaneous specifies changes to mak e to the [Miscellaneous] section of the CPF. InterSystems IRIS® modifies the [Miscellaneous] section of the CPF when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.Miscellaneous.Modify() to modify the [Miscellaneous] section of the CPF defined by ModifyMiscellaneous.

This operation is designed to be used during a configuration mer ge. You can change ModifyMiscellaneous by editing the merge file in a te xt editor (as described in Editing the Active CPF).

ModifyMonitor

ModifyMonitor

Modify the [Monitor] section of the CPF.

Synopsis

[Actions]
ModifyMonitor:[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyMonitor specifies changes to mak e to the [Monitor] section of the CPF. InterSystems IRIS® modifies the [Monitor] section of the CPF when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.Monitor .Modify() to modify the [Monitor] section of the CPF defined by ModifyMonitor.

This operation is designed to be used during a configuration mer ge. You can change ModifyMonitor by editing the merge file in a te xt editor (as described in Editing the Active CPF).

[Actions]

CreateNamespace

Create a new namespace.

Synopsis

[Actions]
CreateNamespace:Name=NamespaceName,Globals=DatabaseName[,Routines=DatabaseName,TempGlobals=DatabaseName,Interop=n]

Description
CreateNamespace defines a namespace. InterSystems IRIS® creates that namespace when processing the [Actions] section during a configuration mer ge.

You can specify any possible namespace properties in the CreateNamespace definition. During the mer ge, InterSystems IRIS runs Config.Namespaces.Create() to create the namespace defined by CreateNamespace. The namespace properties
are:

- Name (required) – The name for the new namespace.

- Globals (required) – The default globals database for the new namespace.

- Routines – The default routines database for the new namespace. If unspecified, this uses the same database as Globals.

- TempGlobals – The default temporary globals database for the new namespace. If unspecified, this uses the database.

- IRISTEMP Interop (1 or 0) – The flag that determines if the ne w namespace is enabled for interoperability. If unspecified, the flag is set to the default value of 0 and the new namespace is not enabled for interoperability. When set to 1, the new namespace is enabled for interoperability.

Example

Upon running a configuration mer ge, the following example creates the Sales namespace, which uses the Sales database for globals and the SYS database for routines.

[Actions]
CreateNamespace:Name=Sales,Globals=Sales,Routines=SYS

This operation is designed to be used during a configuration mer ge. You can change CreateNamespace by editing the merge file in a te xt editor (as described in Editing the Active CPF).

For information about creating and editing a namespace from the Management Portal, see Configuring Namespaces .

DeleteNamespace

DeleteNamespace

Delete an existing namespace.

Synopsis

[Actions] DeleteNamespace:Name=NamespaceName

Description
DeleteNamespace specifies a namespace to delete. InterSystems IRIS® deletes that namespace when processing the [Actions] section during a configuration mer ge.

Specify the Name of the namespace to delete. During the merge, InterSystems IRIS runs Config.Namespaces.Delete() to delete the namespace specified by DeleteNamespace.

This operation is designed to be used during a configuration mer ge. You can change DeleteNamespace by editing the merge file in a te xt editor (as described in Editing the Active CPF).

For information about deleting a namespace from the Management Portal, see Configuring Namespaces .

[Actions]

ModifyNamespace

Modify an existing namespace.

Synopsis

[Actions] ModifyNamespace:Name=NamespaceName[Additional Properties]

Description
ModifyNamespace specifies changes to mak e to a namespace. InterSystems IRIS® modifies the specified namespace when processing the [Actions] section during a configuration mer ge.

You can specify any possible namespace properties in the ModifyNamespace definition. During the mer ge, InterSystems IRIS runs Config.Namespaces.Modify() to modify the namespace as specified by ModifyNamespace. The complete list of namespace properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change ModifyNamespace by editing the merge file in a te xt editor (as described in Editing the Active CPF).

For information about modifying a namespace from the Management Portal, see Configuring Namespaces .

CreateOAuth2Client

CreateOAuth2Client

Create a new OAuth client configuration.

Synopsis

[Actions]
CreateOAuth2Client:ApplicationName=ClientApplicationName,ClientType=ClientType,ServerDefinition=IssuerEndpoint,SSLConfiguration=SSLConfigName[,Additional
Properties]

Where:

- ClientApplicationName identifies the application configuration.

- ClientType is public, confidential, or resource. If ClientType=resource, then the RedirectionEndpoint property is also required.

- IssuerEndpoint is the IssuerEndpoint of the OAuth2.ServerDefinition object which describes the authorization server.

- SSLConfigName is the name of the activated TLS/SSL configuration to use for authorization serv er requests.

Description
server definition has already CreateOAuth2Client defines a client configuration in the security database for when a been set up. InterSystems IRIS® creates the defined client configuration when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs OAuth2.Client.Create() to create the client configuration defined by CreateOAuth2Client.

The required properties for CreateOAuth2Client are ApplicationName, ClientType, ServerDefinition , and SSLConfigur ation, all other properties are optional. The complete list of properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change CreateOAuth2Client by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Creating a Client Configuration for more information on client configurations.

[Actions]

DeleteOAuth2Client

Delete an OAuth client configuration.

Synopsis

[Actions]
DeleteOAuth2Client:ApplicationName=ClientApplicationName

Description
DeleteOAuth2Client deletes an existing client configuration from the security database. InterSystems IRIS® deletes the client configuration when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs OAuth2.Client.DeleteId() to delete the client configuration defined by DeleteOAuth2Client.

This operation is designed to be used during a configuration mer ge. You can change DeleteOAuth2Client by editing the merge file in a te xt editor (as described in Editing the Active CPF).

ModifyOAuth2Client

ModifyOAuth2Client

Modify an OAuth client configuration.

Synopsis

[Actions]
ModifyOAuth2Client:ApplicationName=ClientApplicationName,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyOAuth2Client modifies an e xisting client configuration. InterSystems IRIS® modifies the client configuration when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs OAuth2.Client.Modify() to modify the client description defined by ModifyOAuth2Client.

You can specify any properties from the OAuth2.Client class in the ModifyOAuth2Client definition. The complete list of properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change ModifyOAuth2Client by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Creating a Client Configuration for more information on client configurations.

[Actions]

RegisterOAuth2Client

Register an OAuth client.

Synopsis

[Actions]
RegisterOAuth2Client:ApplicationName=ClientApplicationName

Description
RegisterOAuth2Client registers the client, retrieves client metadata, and then updates the associated client configuration. This action can only be used when the authorization server supports dynamic client registration. InterSystems IRIS® registers the client when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs %SYS.OAuth.Registration.RegisterClient() to register the client defined by RegisterOAuth2Client.

This operation is designed to be used during a configuration mer ge. You can change RegisterOAuth2Client by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See item number 4 in Creating a Client Configuration for more information on registering a client.

CreateOAuth2Server

CreateOAuth2Server

Create a new OAuth authorization server configuration.

Synopsis

[Actions]
CreateOAuth2Server:SupportedScopes=SupportedScopes,IssuerEndpoint=IssuerEndpiont,CustomizationRoles=CustomizationRoles,CustomizaitonNamespace=CustomizationNamespace,SSLConfiguration=SSLConfigName[,Additional
Properties]

Where:

- SupportedScopes is a JSON string of the scopes and description of the scopes. For example:

- {"scope1":"description1","scope2":"description2"}

- IssuerEndpoint is the endpoint for this authorization server, for example https:localhost:80/YourIssuerEndpoint.

- CustomizationRoles is a comma-separated list of roles.

- CustomizationNamespace is the namespace where the customization code will run.

SSLConfigName is the name of the activated TLS/SSL configuration to use loading a request object.

Description
CreateOAuth2Server defines an authorization serv er configuration in the security database. InterSystems IRIS® creates the defined authorization serv er configuration when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs OAuth2.Server.Configuration.Create() to create the authorization serv er defined by
CreateOAuth2Server.

The required properties for CreateOAuth2Server are SupportedScopes, IssuerEndpoint, CustomizationRoles, CustomizationNamespace, and SSLConfigur ation, all other OAuth2.Server.Configuration.Create() properties are optional. The complete list of properties is available in the class reference.

Note:

The SigningAlgorithm, KeyAlgorithm, and EncryptionAlgorithm properties cannot explicitly be set to null ("") using CreateOAuth2Server. If you try to set the preceding properties to null you will receive an error.

This operation is designed to be used during a configuration mer ge. You can change CreateOAuth2Server by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Creating the Authorization Server Configuration for more information on authorization server configurations.

[Actions]

DeleteOAuth2Server

Delete the OAuth authorization server configuration.

Synopsis

[Actions]
DeleteOAuth2Server

Description
DeleteOAuth2Server deletes the existing authorization server configuration from the security database. InterSystems IRIS® deletes the authorization server configuration when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs OAuth2.Server.Configuration.Delete() to delete the authorization serv er configuration. DeleteOAuth2Server has no required properties.

This operation is designed to be used during a configuration mer ge. You can change DeleteOAuth2Server by editing the merge file in a te xt editor (as described in Editing the Active CPF).

ModifyOAuth2Server

ModifyOAuth2Server

Modify the OAuth authorization server configuration.

Synopsis

[Actions]
ModifyOAuth2Server:[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyOAuth2Server modifies the e xisting authorization server configuration. InterSystems IRIS® modifies the authorization server configuration when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs OAuth2.Server.Configuration.Modify() to modify the authorization serv er configuration.

You can specify any properties from the OAuth2.Server.Configuration class in the ModifyOAuth2Server definition. The complete list of properties is available in the class reference.

Note:

The SigningAlgorithm, KeyAlgorithm, and EncryptionAlgorithm properties cannot explicitly be set to null ("") using ModifyOAuth2Server. If you try to set the preceding properties to null you will receive an error.

This operation is designed to be used during a configuration mer ge. You can change ModifyOAuth2Server by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Creating the Authorization Server Configuration for more information on authorization server configurations.

[Actions]

DiscoverOAuth2Server

Discover an OAuth authorization server which is available for client configuration.

Synopsis

[Actions]
DiscoverOAuth2Server:IssuerEndpoint=IssuerEndpoint,SSLConfiguration=SSLConfigName

Where:

- IssuerEndpoint is the endpoint URL used to identify the authorization server.

- SSLConfigName is the SSL configuration used to connect to the authorization serv er.

Description
DiscoverOAuth2Server discovers the server metadata for a client and saves the server metadata in an existing or new OAuth2.ServerDefinition instance. This action should be used on the client to discover the server. InterSystems IRIS® discovers the server when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs %SYS.OAuth2.Registration.Discover() to discover the server defined by DiscoverOAuth2Server.

This operation is designed to be used during a configuration mer ge. You can change DiscoverOAuth2Server by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Creating a Client Configuration for more information.

CreateOAuth2ServerClient

CreateOAuth2ServerClient

Create a new OAuth server client description.

Synopsis

[Actions]
CreateOAuth2ServerClient:Name=Name,ClientId=ClientId,ClientType=ClientType[,Additional Properties]

Where:

- Name is the name of this client

- ClientId is the new unique ID for this client.

- ClientType is public, confidential, or resource.

Description
CreateOAuth2ServerClient defines a client description in the security database for when InterSystems IRIS® has been set up as an authorization server (see Creating the Authorization Server Configuration ). InterSystems IRIS creates the defined client description when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs OAuth2.Server.Client.Create() to create the client description defined by
CreateOAuth2ServerClient.

The required properties for CreateOAuth2ServerClient are Name, ClientId, and ClientType, all other properties are optional. You can specify any possible client description properties in the CreateOAuth2ServerClient definition, including PublicJWKSExpires. The complete list of properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change CreateOAuth2ServerClient by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Creating a Client Description for more information on client descriptions.

[Actions]

DeleteOAuth2ServerClient

Delete an OAuth client description.

Synopsis

[Actions]
DeleteOAuth2ServerClient:ClientId=ClientId

Description
DeleteOAuth2ServerClient deletes an existing client description from the security database. InterSystems IRIS® deletes the client description when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs OAuth2.Server.Client.DeleteId() to delete the client description defined by
DeleteOAuth2ServerClient.

This operation is designed to be used during a configuration mer ge. You can change DeleteOAuth2ServerClient by editing the merge file in a te xt editor (as described in Editing the Active CPF).

ModifyOAuth2ServerClient

ModifyOAuth2ServerClient

Modify an OAuth client description.

Synopsis

[Actions]
ModifyOAuth2ServerClient:ClientId=ClientId,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyOAuth2ServerClient modifies an e xisting client description. InterSystems IRIS® modifies the client description when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs OAuth2.Server.Client.Modify() to modify the client description defined by ModifyOAuth2ServerClient.

You can modify any properties from the OAuth2.Server.Client class except ClientId using ModifyOAuth2ServerClient. The complete list of properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change ModifyOAuth2ServerClient by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Creating a Client Description for more information on client descriptions.

[Actions]

CreateOAuth2ServerDefinition

Create a new OAuth server description.

Synopsis

[Actions]
CreateOAuth2ServerDefinition:IssuerEndpoint=IssuerEndpoint,SSLConfiguration=SSLConfigName,Metadata=MetadataProperties[,Additional
Properties]

Where:

- IssuerEndpoint is the endpoint URL used to identify the authorization server.

- SSLConfigName is the name of the activated TLS/SSL configuration to use for authorization serv er discovery requests.

- MetadataProperties is a JSON string of OAuth2.Server.Metadata properties and values. To set the authorization_endpoint and token_endpoint properties in the OAuth2.Server.Metadata class, they must be
set in this JSON string. For example:

Metadata={"authorization_endpoint":"YourAuthEndpoint","token_endpoint":"YourTokenEndpoint"}

Description
CreateOAuth2ServerDefinition defines a serv er description in the security database. InterSystems IRIS® creates the defined serv er description when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs OAuth2.ServerDefinition.Create() to create the serv er description defined by
CreateOAuth2ServerDefinition.

The required properties for CreateOAuth2ServerDefinition are IssuerEndpoint, SSLConfigur ation, and Metadata;
you may also optionally specify the InitialAccessToken and ServerCredentials properties. The complete list of properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change CreateOAuth2ServerDefinition by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Creating a Server Description for more information on client descriptions.

DeleteOAuth2ServerDefinition

DeleteOAuth2ServerDefinition

Delete an OAuth server description.

Synopsis

[Actions]
DeleteOAuth2ServerDefinition:IssuerEndpoint=IssuerEndpoint

Description
DeleteOAuth2ServerDefinition deletes an existing server description from the security database. InterSystems IRIS® deletes the server description when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs OAuth2.ServerDefinition.DeleteByIssuer() to delete the serv er description defined by
DeleteOAuth2ServerDefinition.

This operation is designed to be used during a configuration mer ge. You can change DeleteOAuth2ServerDefinition by editing the merge file in a te xt editor (as described in Editing the Active CPF).

[Actions]

ModifyOAuth2ServerDefinition

Modify an OAuth server description.

Synopsis

[Actions]
ModifyOAuth2ServerDefinition:IssuerEndpoint=IssuerEndpoint,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyOAuth2ServerDefinition modifies an e xisting server description. InterSystems IRIS® modifies the serv er description when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs OAuth2.ServerDefinition.Modify() to modify the serv er description defined by ModifyOAuth2ServerDefinition.

You can modify the following properties using ModifyOAuth2ServerDefinition: Metadata, InitialAccessToken, SSLConfiguration, and ServerCredentials. Consult the class reference for more information on these properties

This operation is designed to be used during a configuration mer ge. You can change ModifyOAuth2ServerDefinition by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Creating a Server Description for more information on client descriptions.

CreateOAuth2Resource

CreateOAuth2Resource

Create a new OAuth resource server.

Synopsis

[Actions]
CreateOAuth2Resource:Name=Name[,Authenticator=Authenticator,ServerDefinition=ServerDefinition,Audiences=Audiences,AdditionalProperteis]

Where:

- Name is the name of the new resource server.

- Authenticator is a %DynamicObject or a JSON string. What is passed to Authenticator is saved in the Authenticator
property of the OAuth2.ResourceServer class. The default is:

- "{"Implementation":"%OAuth2.ResourceServer.SimpleAuthenticator"}"

- ServerDefinition is the server definition’ s IssuerEndpoint string. The ServerDefintion object must already exist before using it in this action.

Audiences is a comma-delimited string. What is passed to Audiences is saved in the Audiences property of the
OAuth2.ResourceServer class.

Description
CreateOAuth2Resource creates an OAuth resource server. InterSystems IRIS® creates the defined resource serv er when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs OAuth2.ResourceServer.Create() to create the resource server defined by CreateOAuth2Resource.

Only the Name property is required, all other properties are optional, the complete list of properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change CreateOAuth2Resource by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Using an InterSytems IRIS Web Application as an OAuth 2.0 Resource Server for more information.

[Actions]

DeleteOAuth2Resource

Delete an existing OAuth resource server.

Synopsis

[Actions]
DeleteOAuth2Resource:Name=Name

Description
DeleteOAuth2Resource deletes an existing OAuth resource server. InterSystems IRIS® deletes the resource server when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs OAuth2.ResourceServer.Delete() to delete the resource server defined by DeleteOAuth2Resource.

This operation is designed to be used during a configuration mer ge. You can change DeleteOAuth2Resource by editing the merge file in a te xt editor (as described in Editing the Active CPF).

ModifyOAuth2Resource

ModifyOAuth2Resource

Modify an existing OAuth resource server.

Synopsis

[Actions]
ModifyOAuth2Resource:Name=Name,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyOAuth2Resource modifies an e xisting OAuth resource server. InterSystems IRIS® modifies the resource serv er when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs OAuth2.ResourceServer.Modify() to modify the resource server defined by ModifyOAuth2Resource. The complete list of properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change ModifyOAuth2Resource by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Using an InterSytems IRIS Web Application as an OAuth 2.0 Resource Server for more information.

[Actions]

CreateResource

Create a new resource.

Synopsis

[Actions] CreateResource:Name=ResourceName,PublicPermission=[R/W/U]

Description
CreateResource specifies a resource to create. InterSystems IRIS® creates that resource when processing the [Actions] section during a configuration mer ge.

You can specify any possible resource properties in the CreateResource definition. During the mer ge, InterSystems IRIS runs Security.Resources.Create() to create the resource defined by CreateResource. The complete list of resource properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change CreateResource by editing the merge file in a te xt editor (as described in Editing the Active CPF).

For information about creating resources from the Management Portal, see Create or Edit a Resource.

DeleteResource

DeleteResource

Delete an existing resource.

Synopsis

[Actions] DeleteResource:Name=ResourceName

Description
DeleteResource specifies a resource to delete. InterSystems IRIS® deletes that resource when processing the [Actions] section during a configuration mer ge.

Specify the Name of the resource to delete. During the merge, InterSystems IRIS runs Security.Resources.Delete() to delete the resource specified by DeleteResource.

This operation is designed to be used during a configuration mer ge. You can change DeleteResource by editing the merge file in a te xt editor (as described in Editing the Active CPF).

You can also delete resources from the Resources page of the Management Portal (System Administration > Security >
Resources).

[Actions]

ModifyResource

Modify an existing resource.

Synopsis

[Actions] ModifyResource:Name=ResourceName[,Additional Properties]

Description
ModifyResource specifies changes to mak e to a resource. InterSystems IRIS® modifies the specified resource when processing the [Actions] section during a configuration mer ge.

You can specify any possible resource properties in the ModifyResource definition. During the mer ge, InterSystems IRIS runs Security.Resources.Modify() to make the changes defined by ModifyResource. The complete list of resource properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change ModifyResource by editing the merge file in a te xt editor (as described in Editing the Active CPF).

For information about modifying a resource from the Management Portal, see Create or Edit a Resource.

CreateRole

CreateRole

Create a new role.

Synopsis

[Actions] CreateRole:Name=RoleName,Resources=ResourceName(s)[,Additional Properties]

Description
CreateRole defines a role. InterSystems IRIS® creates that role when processing the [Actions] section during a configuration merge.

You can specify any possible role properties in the CreateRole definition. During the mer ge, InterSystems IRIS runs Security.Roles.Create() to create the role defined by CreateRole. The complete list of role properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change CreateRole by editing the merge file in a te xt editor (as described in Editing the Active CPF).

For information about creating roles from the Management Portal, see Create Roles.

[Actions]

DeleteRole

Delete an existing role.

Synopsis

[Actions] DeleteRole:Name=RoleName

Description
DeleteRole specifies a role to delete. InterSystems IRIS® deletes that role when processing the [Actions] section during a configuration mer ge.

Specify the Name of the role to delete. During the merge, InterSystems IRIS runs Security.Roles.Delete() to delete the role specified by DeleteRole.

This operation is designed to be used during a configuration mer ge. You can change DeleteRole by editing the merge file in a te xt editor (as described in Editing the Active CPF).

For information about deleting a role from the Management Portal, see Delete a Role.

ModifyRole

ModifyRole

Modify an existing role.

Synopsis

[Actions] ModifyRole:Name=RoleName[,Additional Properties]

Description
ModifyRole specifies changes to mak e to a role. InterSystems IRIS® modifies the specified role when processing the [Actions] section during a configuration mer ge.

You can specify any possible role properties in the ModifyRole definition. During the mer ge, InterSystems IRIS runs Security.Roles.Modify() to make the changes defined by ModifyRole. The complete list of SPECIFIC properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change ModifyRole by editing the merge file in a te xt editor (as described in Editing the Active CPF).

For information about modifying a role from the Management Portal, see Manage Roles.

[Actions]

ModifyService

Modify a service.

Synopsis

[Actions]
ModifyService:Name=ServiceName,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyService specifies changes to mak e to an existing service. InterSystems IRIS® modifies the e xisting service when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Security.Services.Modify() to modify the existing service defined by ModifyService.

This operation is designed to be used during a configuration mer ge. You can change ModifyService by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Services for more information on services.

ConfigShardedCluster

ConfigShardedCluster

Configure a sharded cluster .

Synopsis

[Actions]
ConfigShardedCluster:ClusterURL=ClusterURL,Role=Role,Member=Member,MasterRegexp=MasterRegexp,Regexp=Regexp,Map=Map,ArbiterURL=ArbiterURL,SSLDir=SSLDir

ClusterURL

Specifies the location of node1; this field should be populated for all v alues of Role other than node1.

Substitute ClusterURL with the cluster’s URL in the format IRIS://host:port/namespace, host:port is the hostname and port of the initial node, namespace is the cluster namespace.

Role

Member

Substitute Role with node1, data, compute, or auto.

Substitute Member with primary, backup, drasync, rorasync, rwrasync, or auto.

MasterRegexp

Used when Role is set to auto.

Default:"-0$"

MasterRegexp identifies the instance to be configured as data node 1 if the name of the instance’ the regular expression that is substituted for MasterRegexp.

s host matches

The default assumes that the hostname for the node1 node ends in "-0".

Regexp

Required for using Role and Member in automatic mode.

Default:"-[0-9]+$"

Regexp validates that the name of the instance’s host matches the regular expression that is substituted for Regexp.

The default value assumes a naming scheme such as iris-data-0, iris-data-1...iris-data-N.

Map

Optional

Default:"primary,backup"

Map is used to specify a mirror map. It requires nodes be named in a manner consistent with Regexp, are ascending, and contiguous.

Substitute Map with a comma-separated list of values to assign members of a mirror set; allowed values are
primary, backup, and drasync.

ArbiterURL

Optional

ArbiterURL is used to specify an Arbiter.

[Actions]

Substitute ArbiterURL with the Host (hostname or IP address) and port of the arbiter with the following format, host:port.

SSLDir

Optional

SSLDir is used to specify the path to the directory containing the Certificate Authority certificate ( CAFile.pem), Server Certificate file ( CertificateFile.pem), and Server Key file ( PrivateKeyFile.pem).

Substitute SSLDir with the desired directory path.

Description
ConfigShardedCluster specifies changes to mak e to a sharded cluster. InterSystems IRIS® modifies the sharded cluster when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs a sequence of API calls to modify the sharded cluster defined by ConfigShardedCluster.

This operation is designed to be used during a configuration mer ge. You can change ConfigShardedCluster by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Sharding for more information on sharding configurations.

GrantPrivilege

GrantPrivilege

Grant an object privilege to a user.

Synopsis

[Actions]
GrantPrivilege:ObjPriv=a,ObjList=b,Type=c,User=d,Namespace=e,WithGrant=f

Where a is a comma-delimited string of actions to grant or * for all actions.

Where b is a comma-delimited list of SQL object names or * for all objects.

Where c is Table, View, Schema, Stored Procedures, or ML Configuration.

Where d is a comma-delimited list of users.

Where e is the desired Namespace.

Where f is 0/1 for the WITH GRANT OPTION.

Description
GrantPrivilege lets you grant an object privilege to users via this call instead of using the SQL GRANT statement. This can include grant privileges. InterSystems IRIS® grants the privileges when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS changes to the provided namespace and runs %SYS- TEM.SQL.Security.GrantPrivilege() (if WithGrant=0) or %SYSTEM.SQL.Security.GrantPrivilegeWithGrant() (if WithGrant=1) to grant the object privileges defined by GrantPrivilege.

This operation is designed to be used during a configuration mer ge. You can change GrantPrivilege by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See GRANT (SQL) for information on the SQL command.

[Actions]

RevokePrivilege

Revoke an object privilege from a user.

Synopsis

[Actions]
RevokePrivilege:ObjPriv=a,ObjList=b,Type=c,User=d,Namespace=e[,AdditionalProperties]

Where a is a comma-delimited string of actions to revoke or * for all actions.

Where b is a comma-delimited list of SQL object names or * for all objects.

Where c is Table, View, Schema, Stored Procedures, or ML Configuration.

Where d is a comma-delimited list of users.

Where e is the desired Namespace.

Description
RevokePrivilege lets you revoke an object privilege from users via this call instead of using the SQL REVOKE statement. InterSystems IRIS® revokes the privileges when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS changes to the desired namespace and runs %SYSTEM.SQL.Security.RevokePrivilege() to revoke the object privileges defined by RevokePrivilege.

This operation is designed to be used during a configuration mer ge. You can change RevokePrivilege by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See REVOKE (SQL) for information on the SQL command.

GrantAdminPrivilege

GrantAdminPrivilege

Grant an SQL administrative privilege to a grantee: a user or role.

Synopsis

[Actions]
GrantAdminPrivilege:AdminPriv=a,Grantee=b,Namespace=c,WithGrant=d

Where a is a comma-delimited list of SQL administrative privileges to grant.

Where b is a comma-delimited list of users or roles.

Where c is the desired Namespace.

Where d is 0/1 for the WITH ADMIN OPTION.

Description
GrantAdminPrivilege lets you grant an SQL administrative privilege to users or roles via this call. InterSystems IRIS® grants the privileges when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS changes to the provided namespace and executes a GRANT (SQL) command to grant the administrative privileges defined by GrantAdminPrivilege.

This operation is designed to be used during a configuration mer ge. You can change GrantAdminPrivilege by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See GRANT (SQL) for information on the SQL command.

[Actions]

RevokeAdminPrivilege

Revoke an SQL administrative privilege from a grantee: a user or role.

Synopsis

[Actions]
RevokeAdminPrivilege:AdminPriv=a,Grantee=b,Namespace=c

Where a is a comma-delimited list of SQL administrative privileges to revoke.

Where b is a comma-delimited list of users or roles.

Where c is the desired Namespace.

Description
RevokeAdminPrivilege lets you revoke an SQL administrative privilege from users or roles via this call. InterSystems IRIS® revokes the privileges when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS changes to the provided namespace and executes a REVOKE (SQL) command to revoke the administrative privileges defined by RevokeAdminPrivilege.

This operation is designed to be used during a configuration mer ge. You can change RevokeAdminPrivilege by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See REVOKE (SQL) for information on the SQL command.

ModifySQL

ModifySQL

Modify the [SQL] section of the CPF.

Synopsis

[Actions]
ModifySQL:[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifySQL specifies changes to mak e to the SQL settings. InterSystems IRIS® modifies the SQL settings when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.SQL.Modify() to modify the SQL settings defined by ModifySQL.

This operation is designed to be used during a configuration mer ge. You can change ModifySQL by editing the merge file in a text editor (as described in Editing the Active CPF).

See [SQL] for information on the [SQL] CPF section.

[Actions]

CreateSqlSysDatatype

Create a SQL system datatype definition in the [SqlSysDatatypes] section of the CPF .

Synopsis

[Actions]
CreateSqlSysDatatype:Name=DatatypeName,Datatype=datatype

Description
CreateSqlSysDatatype defines a SQL system datatype. InterSystems IRIS® creates that SQL system datatype when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.SqlSys- Datatypes.Create() to create the SQL system datatype defined by CreateSqlSysDatatype. The Name and Datatype
properties are required; the complete list of properties is available in the class reference.

Example

The following example demonstrates how to use the CreateSqlSysDatatype operation in the configuration mer ge file:

[Actions]
CreateSqlSysDatatype:Name=BigInt,Datatype=%Library.BigInt

This operation is designed to be used during a configuration mer ge. You can change CreateSqlSysDatatype by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Datatypes (SQL) for more information on SQL datatypes.

DeleteSqlSysDatatype

DeleteSqlSysDatatype

Delete a SQL system datatype definition in the [SqlSysDatatypes] section of the CPF .

Synopsis

[Actions]
DeleteSqlSysDatatype:Name=DatatypeName

Description
DeleteSqlSysDatatype deletes an existing SQL system datatype. InterSystems IRIS® deletes the specified SQL system datatype when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.SqlSysDatatypes.Delete() to delete the SQL system datatype defined by DeleteSqlSysDatatype.

This operation is designed to be used during a configuration mer ge. You can change DeleteSqlSysDatatype by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Datatypes (SQL) for more information on SQL datatypes.

[Actions]

ModifySqlSysDatatype

Modify a SQL system datatype definition in the [SqlSysDatatypes] section of the CPF .

Synopsis

[Actions]
ModifySqlSysDatatype:Name=DatatypeName,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifySqlSysDatatype specifies changes to mak e to an existing SQL system datatype. InterSystems IRIS® modifies the SQL system datatype when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.SqlSysDatatypes.Modify() to modify the SQL system datatype defined by ModifySqlSysDatatype.

This operation is designed to be used during a configuration mer ge. You can change ModifySqlSysDatatype by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Datatypes (SQL) for more information on SQL datatypes.

CreateSqlUserDatatype

CreateSqlUserDatatype

Create a SQL user datatype definition in the [SqlUserDatatypes] section of the CPF .

Synopsis

[Actions]
CreateSqlUserDatatype:Name=Name,Datatype=datatype

Description
CreateSqlUserDatatype defines a SQL user datatype. InterSystems IRIS® creates that SQL user datatype when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.SqlUser - Datatypes.Create() to create the SQL user datatype defined by CreateSqlUserDatatype. The Name and Datatype
properties are required; the complete list of properties is available in the class reference.

Example

The following example demonstrates how to use the CreateSqlUserDatatype operation in the configuration mer ge
file:

[Actions]
CreateSqlUserDatatype:Name=BigNEWInt,Datatype=%Library.BigNEWInt

This operation is designed to be used during a configuration mer ge. You can change CreateSqlUserDatatype by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Datatypes (SQL) for more information on SQL datatypes.

[Actions]

DeleteSqlUserDatatype

Delete a SQL user datatype definition in the [SqlUserDatatypes] section of the CPF .

Synopsis

[Actions]
DeleteSqlUserDatatype:Name=Name

Description
DeleteSqlUserDatatype deletes an existing SQL user datatype. InterSystems IRIS® deletes the specified SQL user datatype when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.SqlUserDatatypes.Delete() to delete the SQL user datatype defined by DeleteSqlUserDatatype.

This operation is designed to be used during a configuration mer ge. You can change DeleteSqlUserDatatype by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Datatypes (SQL) for more information on SQL datatypes.

ModifySqlUserDatatype

ModifySqlUserDatatype

Modify a SQL user datatype definition in the [SqlUserDatatypes] section of the CPF .

Synopsis

[Actions]
ModifySqlUserDatatype:Name=DatatypeName,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifySqlUserDatatype specifies changes to mak e to an existing SQL user datatype. InterSystems IRIS® modifies the SQL user datatype when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.SqlUserDatatypes.Modify() to modify the SQL user datatype defined by ModifySqlUserDatatype.

This operation is designed to be used during a configuration mer ge. You can change ModifySqlUserDatatype by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Datatypes (SQL) for more information on SQL datatypes.

[Actions]

CreateSSLConfig

Create a TLS configuration.

Synopsis

[Actions]
CreateSSLConfig:Name=SSLConfigName[,AdditionalProperties]

Description
CreateSSLConfig defines a TLS configuration. InterSystems IRIS® creates that TLS configuration when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Security.SSLConfigs.Create()
to create the TLS configuration defined by CreateSSLConfig. The Name property is required; the complete list of
properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change CreateSSLConfig by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See About TLS Configurations for more information on TLS configurations.

DeleteSSLConfig

DeleteSSLConfig

Delete a TLS configuration.

Synopsis

[Actions]
DeleteSSLConfig:Name=SSLConfigName

Description
DeleteSSLConfig deletes an existing TLS configuration. InterSystems IRIS® deletes the specified TLS configuration when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Security.SSLConfigs.Delete() to delete the TLS configuration defined by DeleteSSLConfig.

This operation is designed to be used during a configuration mer ge. You can change DeleteSSLConfig by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See About TLS Configurations for more information on TLS configurations.

[Actions]

ModifySSLConfig

Modify a TLS configuration.

Synopsis

[Actions]
ModifySSLConfig:Name=SSLConfigName,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifySSLConfig specifies changes to mak e to an existing TLS configuration. InterSystems IRIS® modifies the TLS configuration when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Security.SSLConfigs.Modify() to modify the TLS configuratione defined by ModifySSLConfig.

This operation is designed to be used during a configuration mer ge. You can change ModifySSLConfig by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See About TLS Configurations for more information on TLS configurations.

ModifyStartup

ModifyStartup

Modify the [Startup] section of the CPF.

Synopsis

[Actions]
ModifyStartup:[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyStartup specifies changes to mak e to the startup settings. InterSystems IRIS® modifies the startup settings when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.Startup.Modify() to modify the startup settings defined by ModifyStartup.

This operation is designed to be used during a configuration mer ge. You can change ModifyStartup by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Memory and Startup Settings for more information on startup settings.

[Actions]

CreateServer

Create a superserver configuration.

Synopsis

[Actions]
CreateServer:Port=n,BindAddress=bindaddress[,AdditionalProperties]

Where n is an open port number on the system.

Description
CreateServer defines a superserv er configuration. InterSystems IRIS® creates that superserv er configuration when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Security.Servers.Cre-
ate() to create the superserver configuration defined by CreateServer. The Port and BindAddress properties are required;
the complete list of properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change CreateServer by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Managing Superservers for more information on superserver configurations.

DeleteServer

DeleteServer

Delete a superserver configuration.

Synopsis

[Actions]
DeleteServer:Port=n,BindAddress=bindaddress

Where n is the existing superserver’s port number on the system.

Description
DeleteServer deletes an existing superserver configuration. InterSystems IRIS® deletes the specified superserv er configuration when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Security.Servers.Delete() to delete the superserver configuration defined by DeleteServer. The Port and BindAddress properties are required to identify the superserver.

This operation is designed to be used during a configuration mer ge. You can change DeleteServer by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Managing Superservers for more information on superserver configurations.

[Actions]

ModifyServer

Modify a superserver configuration.

Synopsis

[Actions]
ModifyServer:Port=n,BindAddress=bindaddress,[PropertiesToModify]

Where n is the existing superserver’s port number on the system.

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyServer specifies changes to mak e to an existing superserver configuration. InterSystems IRIS® modifies the superserver configuration when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Security.Servers.Modify() to modify the superserver configuration defined by ModifyServer.

This operation is designed to be used during a configuration mer ge. You can change ModifyServer by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Managing Superservers for more information on superserver configurations.

ModifySystem

ModifySystem

Modify system security settings.

Synopsis

[Actions]
ModifySystem:Name=Name,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifySystem specifies changes to mak e to the system security settings. InterSystems IRIS® modifies the system security settings when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Security.System.Modify() to modify the system security settings defined by ModifySystem.

This operation is designed to be used during a configuration mer ge. You can change ModifySystem by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See System-Wide Security Parameters for more information on system security settings.

[Actions]

CreateUser

Create a new user account.

Synopsis

[Actions] CreateUser:Name=UserName,PasswordHash="PasswordHashDetails",Roles="RolesList"[,Additional
Properties]

Description
CreateUser defines a user account. InterSystems IRIS® creates that user account when processing the [Actions] section during a configuration mer ge.

The CreateUser action accepts any of the user account properties which are listed as parameters in the class reference for the Security.Users.Create() method, except Password. Instead, CreateUser accepts a comma-delimited PasswordHash string which contains the hashed password, a salt, and (optionally) the hash function’s work factor and algorithm.

Note:

Because generating hash values manually is time-consuming and error-prone, InterSystems recommends using the passwordhash nanocontainer to generate the PasswordHash string. InterSystems provides this nanocontainer through the InterSystems Container Registry.

The configuration mer ge documentation’s section about creating, modifying and deleting security objects provides an example of how to use the CreateUser action.

This operation is designed to be used during a configuration mer ge. You can change CreateUser by editing the merge file in a te xt editor (as described in Editing the Active CPF).

For information about creating user accounts from the Management Portal, see Create a New User Account.

DeleteUser

DeleteUser

Delete an existing user account.

Synopsis

[Actions] DeleteUser:Name=UserName

Description
DeleteUser specifies a user account to delete. InterSystems IRIS® deletes that account when processing the [Actions] section during a configuration mer ge.

Specify the Name of the user to delete. During the merge, InterSystems IRIS runs Security.Users.Delete() to delete the user specified by DeleteUser.

This operation is designed to be used during a configuration mer ge. You can change DeleteUser by editing the merge file in a te xt editor (as described in Editing the Active CPF).

For information about deleting user accounts from the Management Portal, see Delete a User Account.

[Actions]

ModifyUser

Modify an existing user account.

Synopsis

[Actions] ModifyUser:Name=UserName[,Additional Properties]

Description
ModifyUser specifies changes to mak e to a user account. InterSystems IRIS® modifies the specified user account when processing the [Actions] section during a configuration mer ge.

You can specify any possible user properties in the ModifyUser definition. During the mer ge, InterSystems IRIS runs Security.Users.Modify() to make the changes defined by ModifyUser.

This operation is designed to be used during a configuration mer ge. You can change ModifyUser by editing the merge file in a te xt editor (as described in Editing the Active CPF).

For information about modifying user accounts from the Management Portal, see Manage User Accounts.

CreateWalletCollection

CreateWalletCollection

Creates a collection.

Synopsis

[Actions]
CreateWalletCollection:Name="CollectionName",Privileges

Where Privileges is one of the following, which determines privileges required to edit the collection and use its secrets. The Permission property is optional and, if omitted, defaults to READ for the UseResource and WRITE for the
EditResource:

- UseResource="UseResourceName",EditResource="UseResourceName"

- Resource="ResourceName:Permission": Setting the Resource property sets both the UseResource and the EditResource to the same resource.

Description
CreateWalletCollection creates a new collection. InterSystems IRIS® creates this collection when processing the [Actions] section during a configuration mer ge.

The CreateWalletCollection action accepts any of the properties listed as parameters in the class reference for the
%Wallet.Collection.Create() method.

This operation is designed to be used during a configuration mer ge. You can change CreateWalletCollection by editing the merge file in a te xt editor (as described in Editing the Active CPF).

[Actions]

ModifyWalletCollection

Modifies a collection.

Synopsis

[Actions]
ModifyWalletCollection:Name="CollectionName"[,Privileges]

Where Privileges is one of the following, which determines privileges required to edit the collection and use its secrets. The Permission property is optional and, if omitted, defaults to READ for the UseResource and WRITE for the
EditResource:

- UseResource="UseResourceName",EditResource="UseResourceName"

- Resource="ResourceName:Permission": Setting the Resource property sets both the UseResource and the EditResource to the same resource.

Description
ModifyWalletCollection lets you set rename a collection or change its associated resources. InterSystems IRIS® modifies the collection when processing the [Actions] section during a configuration mer ge.

The ModifyWalletCollection action accepts any of the properties listed as parameters in the class reference for the
%Wallet.Collection.Modify() method.

This operation is designed to be used during a configuration mer ge. You can change ModifyWalletCollection by editing the merge file in a te xt editor (as described in Editing the Active CPF).

DeleteWalletCollection

DeleteWalletCollection

Deletes a collection.

Synopsis

[Actions]
DeleteWalletCollection:Name="CollectionName"

Description
DeleteWalletCollection deletes a collection. InterSystems IRIS® deletes the collection when processing the [Actions] section during a configuration mer ge.

The DeleteWalletCollection action accepts any of the properties listed as parameters in the class reference for the
%Wallet.Collection.Delete() method.

This operation is designed to be used during a configuration mer ge. You can change DeleteWalletCollection by editing the merge file in a te xt editor (as described in Editing the Active CPF).

[Actions]

CreateWalletKeyValue

Creates a key-value pair secret.

Synopsis

[Actions]
CreateWalletKeyValue:Name="CollectionName.SecretName",RequireTLS="Boolean",AllowedHosts="HostList",Secret="secret",Usage="Protocols"

Where:

- CollectionName.SecretName: The name of the secret and the collection to which it belongs.

- Boolean: 1 (true) or 0 (false), whether TLS is required when the secret is transferred through an HTTP request.

- AllowedHosts: A list of hosts that can receive the secret. This only applies if RequireTLS is true.

- Secret: One or more key-value pairs. For example:

- {"user":"myuser","password":"mypassword"} Protocols: A comma-delimited string or dynamic array specifying how the secret can be used. This can be any combi-
nation of the following:

–

–

–

–

HTTP: Secret can be used with %Net.HttpRequest.UseSecret().

SOAP: Secret can be used with %SOAP.WebClient.UseSecret().

SQL: SQL Gateway connections

CUSTOM: Allows you to retrieve the secret directly with %Wallet.KeyValue.GetSecretValue() (requires
%Admin_Wallet:U)

Description
CreateWalletKeyValue creates a new key-value pair secret inside the specified collection. InterSystems IRIS® creates this secret when processing the [Actions] section during a configuration mer ge.

The CreateWalletKeyValue action accepts any of the properties listed as parameters in the class reference for the
%Wallet.KeyValue.Create() method.

This operation is designed to be used during a configuration mer ge. You can change CreateWalletKeyValue by editing the merge file in a te xt editor (as described in Editing the Active CPF).

ModifyWalletKeyValue

ModifyWalletKeyValue

Modifies a key-value pair secret.

Synopsis

[Actions]
ModifyWalletKeyValue:Name="CollectionName.SecretName",RequireTLS=Boolean,AllowedHosts=HostList,Secret="secret",Usage=Protocols

Where:

- CollectionName.SecretName: The name of the secret and the collection to which it belongs.

- Boolean: Whether TLS is required when the secret is transferred through an HTTP request.

- AllowedHosts: A list of hosts that can receive the secret. This only applies if RequireTLS is true.

- Secret: One or more key-value pairs. For example:

- {"user":"myuser","password":"mypassword"} Protocols: A comma-delimited string or dynamic array specifying how the secret can be used. This can be any combi-
nation of the following:

–

–

–

–

HTTP: Secret can be used with %Net.HttpRequest.UseSecret().

SOAP: Secret can be used with %SOAP.WebClient.UseSecret().

SQL: SQL Gateway connections

CUSTOM: Allows you to retrieve the secret directly with %Wallet.KeyValue.GetSecretValue() (requires
%Admin_Wallet:U)

Description
ModifyWalletKeyValue modifies a key-value pair secret. InterSystems IRIS® performs the modification when processing the [Actions] section during a configuration mer ge.

The ModifyWalletKeyValue action accepts any of the properties listed as parameters in the class reference for the
%Wallet.KeyValue.Modify() method.

This operation is designed to be used during a configuration mer ge. You can change ModifyWalletKeyValue by editing the merge file in a te xt editor (as described in Editing the Active CPF).

[Actions]

DeleteWalletKeyValue

Deletes a key-value pair secret.

Synopsis

[Actions]
DeleteWalletKeyValue:Name="CollectionName.SecretName"

Description
DeleteWalletKeyValue deletes an key-value pair secret from the specified collection. InterSystems IRIS® deletes this secret when processing the [Actions] section during a configuration mer ge.

The DeleteWalletKeyValue action accepts any of the properties listed as parameters in the class reference for the
%Wallet.KeyValue.Delete() method.

This operation is designed to be used during a configuration mer ge. You can change DeleteWalletKeyValue by editing the merge file in a te xt editor (as described in Editing the Active CPF).

CreateWalletSymmetricKey

CreateWalletSymmetricKey

Creates a new symmetric key secret.

Synopsis

[Actions]
CreateWalletSymmetricKey:Name="CollectionName.SecretName"[,KeyInfo]

Where:

- CollectionName.SecretName: The name of the secret and the collection to which it belongs.

- KeyInfo: Key information, which can be one of the following:

–

–

–

Length=KeyLength: The length, in bytes, of the symmetric key to generate.

Secret=KeyContents: The key to import.

Secret64=KeyContentsBase64: The Base64-encoded key to import.

Description
CreateWalletSymmetricKey creates a symmetric key secret inside the specified collection. InterSystems IRIS® creates this secret when processing the [Actions] section during a configuration mer ge.

The CreateWalletSymmetricKey action accepts any of the properties listed as parameters in the class reference for the %Wallet.SymmetricKey.Create() method.

This operation is designed to be used during a configuration mer ge. You can change CreateWalletSymmetricKey by editing the merge file in a te xt editor (as described in Editing the Active CPF).

[Actions]

ModifyWalletSymmetricKey

Modifies a ne w symmetric key secret.

Synopsis

[Actions]
ModifyWalletSymmetricKey:Name="CollectionName.SecretName"[,KeyInfo]

Where:

- CollectionName.SecretName: The name of the secret and the collection to which it belongs.

- KeyInfo: Key information, which can be one of the following:

–

–

–

Length=KeyLength: The length, in bytes, of the symmetric key to generate.

Secret=KeyContents: The key to import.

Secret64=KeyContentsBase64: The Base64-encoded key to import.

Description
ModifyWalletSymmetricKey modifies a ne w symmetric key secret inside the specified collection. InterSystems IRIS® modifies this secret when processing the [Actions] section during a configuration mer ge.

The ModifyWalletSymmetricKey action accepts any of the properties listed as parameters in the class reference for the %Wallet.SymmetricKey.Modify() method.

This operation is designed to be used during a configuration mer ge. You can change ModifyWalletSymmetricKey by editing the merge file in a te xt editor (as described in Editing the Active CPF).

DeleteWalletSymmetricKey

DeleteWalletSymmetricKey

Deletes a symmetric key secret.

Synopsis

[Actions]
DeleteWalletSymmetricKey:Name="CollectionName.SecretName"

Description
DeleteWalletSymmetricKey deletes a symmetric key secret from the specified collection. InterSystems IRIS® deletes this secret when processing the [Actions] section during a configuration mer ge.

The DeleteWalletSymmetricKey action accepts any of the properties listed as parameters in the class reference for the %Wallet.SymmetricKey.Delete() method.

This operation is designed to be used during a configuration mer ge. You can change DeleteWalletSymmetricKey by editing the merge file in a te xt editor (as described in Editing the Active CPF).

[Actions]

CreateWalletRSA

Creates an RSA secret.

Synopsis

[Actions]
CreateWalletRSA:Name="CollectionName.SecretName",PublicKey[,AdditionalProperties]

Where:

- PublicKey: The public key. You can provide this in any of the following ways:

– CollectionName.SecretName: The name of the secret and the collection to which it belongs.

–

–

–

Length="KeyPairLength": The length, in bytes, of the RSA public-private key pair to generate. If you specify this property, you should not specify the private key in the AdditionalProperties.

PublicKey="KeyContents": A PEM-encoded public key.

PublicKeyFile="KeyFilePath": The file path of a PEM-encoded public k ey file.

– Certificate=" CertificateContents ": A PEM-encoded certificate.

– CertificateFile=" CertificateF ilePath": The file path of a PEM-encoded certificate file.

- AdditionalProperties can be any of the following:

–

–

–

PrivateKey="KeyContents": A PEM-encoded private key.

PrivateKeyKeyFile="KeyFilePath": The file path of a PEM-encoded pri vate key file.

Password="DecryptionPassword": A password for decrypting password-protected private keys.

Description
CreateWalletRSA creates a RSA secret inside the specified collection. InterSystems IRIS® creates this secret when processing the [Actions] section during a configuration mer ge.

The CreateWalletRSA action accepts any of the properties listed as parameters in the class reference for the %Wallet.RSA.Create() method.

This operation is designed to be used during a configuration mer ge. You can change CreateWalletRSA by editing the merge file in a te xt editor (as described in Editing the Active CPF).

ModifyWalletRSA

ModifyWalletRSA

Modifies an RSA secret.

Synopsis

[Actions]
ModifyWalletRSA:Name="CollectionName.SecretName",PublicKey[,AdditionalProperties]

Where:

- PublicKey: The public key. You can provide this in any of the following ways:

– CollectionName.SecretName: The name of the secret and the collection to which it belongs.

–

–

–

Length="KeyPairLength": The length, in bytes, of the RSA public-private key pair to generate. If you specify this property, you should not specify the private key in the AdditionalProperties.

PublicKey="KeyContents": A PEM-encoded public key.

PublicKeyFile="KeyFilePath": The file path of a PEM-encoded public k ey file.

– Certificate=" CertificateContents ": A PEM-encoded certificate.

– CertificateFile=" CertificateF ilePath": The file path of a PEM-encoded certificate file.

- AdditionalProperties can be any of the following:

–

–

–

PrivateKey="KeyContents": A PEM-encoded private key.

PrivateKeyKeyFile="KeyFilePath": The file path of a PEM-encoded pri vate key file.

Password="DecryptionPassword": A password for decrypting password-protected private keys.

Description
ModifyWalletRSA deletes a RSA secret inside the specified collection. InterSystems IRIS® modifies this secret when processing the [Actions] section during a configuration mer ge.

The ModifyWalletRSA action accepts any of the properties listed as parameters in the class reference for the %Wallet.RSA.Modify() method.

This operation is designed to be used during a configuration mer ge. You can change ModifyWalletRSA by editing the merge file in a te xt editor (as described in Editing the Active CPF).

[Actions]

DeleteWalletRSA

Deletes an RSA secret.

Synopsis

[Actions]
DeleteWalletRSA:Name="CollectionName.SecretName"

Description
DeleteWalletRSA deletes an RSA secret from the specified collection. InterSystems IRIS® deletes this secret when processing the [Actions] section during a configuration mer ge.

The DeleteWalletRSA action accepts any of the properties listed as parameters in the class reference for the %Wallet.RSA.Delete() method.

This operation is designed to be used during a configuration mer ge. You can change DeleteWalletRSA by editing the merge file in a te xt editor (as described in Editing the Active CPF).

CreateWebAppPctAccess

CreateWebAppPctAccess

Creates a percent-class access rule.

Synopsis

[Actions]
CreateWebAppPctAccess:Name=WebApplicationName,AllowType=Type,Class=ClassName,AllowAccess=AllowStatus

Where:

- WebApplicationName: The name of the web application.

- Type: Controls whether the rule applies to the application's access to just the specified class ( AllowClass) or all classes that contain the specified prefix ( AllowPrefix).

- ClassName: The percent class or prefix to gi ve the application access to.

- AllowStatus: Boolean, whether to grant (1) or deny (0) the application access to the specified class or package.

Description
CreateWebAppPctAccess lets you manage a web application's access to additional percent classes.

This operation is designed to be used during a configuration mer ge. You can change CreateWebAppPctAccess by editing the merge file in a te xt editor (as described in Editing the Active CPF).

[Actions]

ModifyWebAppPctAccess

Modifies a percent-class access rule.

Synopsis

[Actions]
ModifyWebAppPctAccess:Name=WebApplicationName,AllowType=Type,Class=ClassName,AllowAccess=AllowStatus

Where:

- WebApplicationName: The name of the web application.

- Type: Controls whether the rule applies to the application's access to just the specified class ( AllowClass) or all classes that contain the specified prefix ( AllowPrefix).

- ClassName: The percent class or prefix to gi ve the application access to.

- AllowStatus: Boolean, whether to grant (1) or deny (0) the application access to the specified class or package.

Description
ModifyWebAppPctAccess lets you manage a web application's access to additional percent classes.

This operation is designed to be used during a configuration mer ge. You can change ModifyWebAppPctAccess by editing the merge file in a te xt editor (as described in Editing the Active CPF).

DeleteWebAppPctAccess

DeleteWebAppPctAccess

Deletes a percent-class access rule.

Synopsis

[Actions]
DeleteWebAppPctAccess:Name=WebApplicationName,AllowType=Type,Class=ClassName

Where:

- WebApplicationName: The name of the web application.

- Type: Controls whether the rule applies to the application's access to just the specified class ( AllowClass) or all classes that contain the specified prefix ( AllowPrefix).

- ClassName: The percent class or prefix to gi ve the application access to.

Description
DeleteWebAppPctAccess deletes a percent-class access rule.

This operation is designed to be used during a configuration mer ge. You can change DeleteWebAppPctAccess by editing the merge file in a te xt editor (as described in Editing the Active CPF).

[Actions]

CreateWorkQueue

Create a work queue definition in the [W orkQueues] section of the CPF.

Synopsis

[Actions]
CreateWorkQueue:Name=WQName,MaxActiveWorkers=n

Where n is a positive integer (>= 0) representing the maximum number of active worker jobs kept in the pool of jobs servicing requests. Default is 0 which represents a dynamic maximum.

Description
CreateWorkQueue defines a w ork queue. InterSystems IRIS® creates that work queue when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.W orkQueues.Create() to create
the work queue defined by CreateWorkQueue. The Name and MaxActiveWorkers properties are required; the complete
list of properties is available in the class reference.

This operation is designed to be used during a configuration mer ge. You can change CreateWorkQueue by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Introduction to the Work Queue Manager for more information on work queues.

DeleteWorkQueue

DeleteWorkQueue

Delete a work queue definition in the [W orkQueues] section of the CPF.

Synopsis

[Actions]
DeleteWorkQueue:Name=WQName

Description
DeleteWorkQueue deletes an existing work queue. InterSystems IRIS® deletes the specified w ork queue when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.W orkQueues.Delete() to delete the work queue defined by DeleteWorkQueue.

This operation is designed to be used during a configuration mer ge. You can change DeleteWorkQueue by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Introduction to the Work Queue Manager for more information on work queues.

[Actions]

ModifyWorkQueue

Modify a work queue definition in the [W orkQueues] section of the CPF.

Synopsis

[Actions]
ModifyWorkQueue:Name=WQName,[PropertiesToModify]

Where you substitute [PropertiesToModify] for the properties you want to modify.

Description
ModifyWorkQueue specifies changes to mak e to a work queue. InterSystems IRIS® modifies the w ork queue when processing the [Actions] section during a configuration mer ge. During the merge, InterSystems IRIS runs Config.W orkQueues.Modify() to modify the work queue defined by ModifyWorkQueue.

This operation is designed to be used during a configuration mer ge. You can change ModifyWorkQueue by editing the merge file in a te xt editor (as described in Editing the Active CPF).

See Introduction to the Work Queue Manager for more information on work queues.

Describes the order in which all Actions are processed during a configuration mer ge.

Description
When InterSystems IRIS® performs a configuration mer ge, Actions are processed in the following order:

- ImportLocale

- InstallLocale

- CreateSSLConfig

- ModifySSLConfig

- ModifySystem

- DeleteSSLConfig

- ModifyEvent

- DeleteEvent

- CreateEvent

- ModifyMonitor

- ModifyJournal

- ModifyConfig

- ModifyECP

- ModifyMiscellaneous

- ModifySQL

- DeleteWorkQueue

- CreateWorkQueue

- ModifyWorkQueue

- ModifyLogging

- DeleteDevice

- DeleteDeviceSubType

- CreateDeviceSubType

- CreateDevice

- ModifyDeviceSubType

- ModifyDevice

- DeleteMagTape

- CreateMagTape

- ModifyMagTape

- DeleteComPort

- CreateComPort

- ModifyComPort

[Actions]

- DeleteSqlSysDatatype

- CreateSqlSysDatatype

- ModifySqlSysDatatype

- DeleteSqlUserDatatype

- CreateSqlUserDatatype

- ModifySqlUserDatatype

- DeleteLicenseServer

- CreateLicenseServer

- ModifyLicenseServer

- DeleteMapPackage

- DeleteMapGlobal

- DeleteMapRoutine

- DeleteNamespace

- DeleteDatabase

- DeleteDatabaseFile

- DeleteECPServer

- DeleteResource

- CreateECPServer

- ModifyECPServer

- ConfigMirror

- CreateResource

- CreateDatabaseFile

- ModifyDatabaseFile

- CreateDatabase

- ModifyDatabase

- CreateNamespace

- ModifyNamespace

- CreateMapPackage

- CreateMapGlobal

- CreateMapRoutine

- ModifyMapPackage

- ModifyMapGlobal

- ModifyMapRoutine

- ModifyMirrorMember

- DeleteMirror

- CreateMirror

- ModifyMirror

- ModifyStartup

- ModifyService

- DeleteUser

- DeleteLDAPConfig

- DeleteWebAppPctAccess

- DeleteApplication

- DeleteDocDB

- DeleteRole

- DeleteServer

- DeleteOAuth2Server

- DeleteOAuth2ServerClient

- DeleteOAuth2ServerDefinition

- DeleteOAuth2Client

- DeleteOAuth2Resource

- CreateServer

- CreateRole

- CreateDocDB

- CreateLDAPConfig

- CreateUser

- CreateApplication

- CreateWebAppPctAccess

- CreateOAuth2Server

- CreateOAuth2ServerClient

- CreateOAuth2ServerDefinition

- DiscoverOAuth2Server

- CreateOAuth2Client

- RegisterOAuth2Client

- CreateOAuth2Resource

- ModifyResource

- ModifyRole

- ModifyServer

- ModifyDocDB

- ModifyLDAPConfig

- ModifyUser

- ModifyApplication

[Actions]

- ModifyWebAppPctAccess

- ModifyOAuth2Server

- ModifyOAuth2ServerClient

- ModifyOAuth2ServerDefinition

- ModifyOAuth2Client

- ModifyOAuth2Resource

- RevokePrivilege

- RevokeAdminPrivilege

- GrantPrivilege

- GrantAdminPrivilege

- ConfigShardedCluster

- CreateWalletCollection

- ModifyWalletCollection

- DeleteWalletCollection

- CreateWalletKeyValue

- ModifyWalletKeyValue

- DeleteWalletKeyValue

- CreateWalletSymmetricKey

- ModifyWalletSymmetricKey

- DeleteWalletSymmetricKey

- CreateWalletRSA

- ModifyWalletRSA

- DeleteWalletRSA

- ConfigProduction

- Execute

[Archives]

The configuration parameter file may include an [Archi in configuring journal settings .

ves] section, which defines optional journal archive targets for use

[Archives]

Archives

Define a journal archi ve target for use in configuring journal settings .

Synopsis

[Archives] Name=Type,Location

Name, Type, and Location are strings.

Description
The [Archives] section of the configuration parameter (CPF) file contains an entry for e
defined for this instance, as follo ws:

very journal archive target

- Name is the name of the archive target.

- Type is the type of the target, one of the following:

–

–

s3 specifies an AWS S3 location.

rsync specifies an on-premises location that uses either a UNIX® or a Windows directory specification. (Note that if the location is a Windows directory, the system actually uses robocopy rather than rsync when it copies the files.)

- Location specifies the actual directory . The format depends on Type.

–

For s3, Location has the following format:

s3://directoryname/

For example:

s3://test-dir/journal-archives/

–

For rsync, Location can any of the following formats:

- For UNIX®:

/directoryname/

Or:

server:/directoryname/

- For Windows:

\\server\share\directoryname\

Or:

drive:\directoryname\

For more information, see Configuring Journal Archive Targets and Configuring Journal Settings .

On the Archive Targets page of the Management Portal (System Administration > Configuration > System Configuration > Archive Targets), to add a new entry, select Create New Archive Target. To edit an existing entry, select Edit in that entry's row.

Archives

[ComPorts]

This topic describes the parameters found in the [ComPorts] section of the CPF. The [ComPorts] section applies to Windows systems only.

[ComPorts]

COMn

Define def ault settings for COM ports. Windows systems only.

Synopsis

[ComPorts] COMn=a,b

Description
The [ComPorts] section contains an entry for each COM port. These entries define the def ault settings for COM ports and enable remote logins to InterSystems IRIS® data platform through locally connected or modem-connected serial ports. The number n refers to the physical COM port number. If the [ComPorts] section contains enough entries, n can be multiple digits.

The parameters within COMn apply to Windows platforms only.

Each COMn entry provides two comma-separated values that define the follo wing default settings for COM port number n:

- a — A set of COM port control parameters (data bits, parity, etc.) in byte-positional format. Byte position is one-based.
From left to right:

Byte Position

Byte 1: Modem Control

Byte 2: Data Bits

Byte 3: Parity

Byte 4: Stop Bits

Byte 5: Flow Control

Byte 6: DTR Setting

Byte 7: $ZA Error Reporting

Description

COMn

–

–

–

–

–

–

–

–

–

–

–

–

–

–

–

–

–

–

–

–

–

–

–

–

–

–

–

–

'1' Use modem control

'0' Do not use modem control

' ' No change to modem control (default)

'5' 5 data bits

'6' 6 data bits

'7' 7 data bits

'8' 8 data bits

' ' No change to bit size (default)

'0' No parity

'1' Odd parity

'2' Even parity

'3' Mark parity

'4' Space parity

' ' No change to the parity setting (default)

'1' 1 stop bit

'5' 1.5 stop bits

'2' 2 stop bits

' ' No change to the stop bit setting (default)

'X' Use Xon/Xoff flow control

'C' Use CTS/RTS flow control

'D' Use DSR/DTR flow control

' ' No change to flow control (default)

'0' Disable DTR (set it off, keep it off)

'1' Enable DTR (set it on, keep it on)

' ' No change to the DTR state (default)

'0' Disable $ZA error reporting

'1' Enable $ZA error reporting

' ' No change to $ZA error reporting (this is
the default)

- b — The baud rate. If not supplied, the default baud rate is 19200.

[ComPorts]

Examples

The following example of a [ComPorts] section shows how spaces can be used as values within the COM port control
parameter:

[ComPorts]
COM1= ,19200

The first e xample uses all defaults for the COM port control parameters by providing seven spaces before the comma separator. The meaning is: No change to modem control, no change to bit size, no change to parity setting, no change to stop
bit setting, no change to Flow control, no change to DTR state, no change to $ZA error reporting.

[ComPorts]
COM2=1801X11,19200

The second example provides a value of 1801X11 for the COM port control parameters. The meaning is: Use modem
control, 8 data bits, no parity, 1 stop bit, use Xon/Xoff flo w control, enable DTR, enable $ZA error reporting.

You can change this parameter in the following ways:

- Edit the CPF in a text editor (as described in Editing the Active CPF).

- Use the Config.ComPorts class.

[config]

This topic describes the Advanced Memory Settings parameters found in the [config] section of the CPF .

[config]

LibPath

Add directories to the LD_LIBRARY_PATH environment variable. UNIX® systems only.

Synopsis

[config] LibPath=directory:directory2:directory3[...]

directory is the full path to a valid directory. The maximum length for LibPath is 1024 characters. By default, no directories are listed.

Description
LibPath is used for UNIX® systems only. This parameter adds a list of directories to the LD_LIBRARY_PATH environment variable (DYLD_LIBRARY_PATH on macOS), which is used to search for third-party shared libraries. If you modify this setting, you must restart the instance for the change to take effect.

On macOS, if you have enabled System Integrity Protection (SIP), it may ignore the DYLD_LIBRARY_PATH variable executing programs in the system directories.

On the Advanced Memory page of the Management Portal (System Administration > Configuration > Additional Settings > Advanced Memory), in the LibPath row, select Edit. Enter one or more directories, separated by colons.

Instead of using the Management Portal, you can change LibPath in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

MaxServerConn

MaxServerConn

Set the maximum number of connections from ECP clients.

Synopsis

[config] MaxServerConn=n

n is an integer in the range 0—254. The default value is 1.

Description
MaxServerConn is the maximum number of ECP clients that can access this instance simultaneously. This is the maximum number of connections that this instance may accept when acting as an ECP server. If you modify this setting, you must restart the instance for the change to take effect.

When an instance is a member of a sharded cluster, this setting must be equal to or greater than the number of nodes in the cluster.

On the Advanced Memory page of the Management Portal (System Administration > Configuration > Additional Settings > Advanced Memory), in the MaxServerConn row, select Edit. Enter a value.

You can also change this parameter on the ECP Settings page of the Management Portal (System Administration > Configuration > Connectivity > ECP Settings). In the This System as an ECP Data Server column, edit Maximum number of application servers.

Instead of using the Management Portal, you can change MaxServerConn in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[config]

MaxServers

Set the maximum number of connections to ECP servers.

Synopsis

[config] MaxServers=n

n is an integer in the range 0—254. The default value is 2.

Description
MaxServers is the maximum number of ECP servers that can be accessed from this instance. This is the maximum number of connections that this instance can establish when acting as an ECP client. If you modify this setting, you must restart the instance for the change to take effect.

When an instance is a member of a mirror configuration , this setting must be identical on all failover mirror members and disaster recovery (DR) asynchronous mirror members. Automatic failover will not occur when this value is not identical on all mirror members.

When an instance is a member of a sharded cluster, this setting must be equal to or greater than the number of nodes in the cluster.

On the page System Administration > Configuration > Connectivity > ECP Settings, in the This System as an ECP Application Server column, edit Maximum number of data servers.

Instead of using the Management Portal, you can change MaxServers in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

Path

Path

Add directories to the default path environment variable. UNIX® systems only.

Synopsis

[config] Path=directory:directory2:directory3[...]

directory is the full path to a valid directory. The maximum length for Path is 1024 characters. By default, no directories are listed.

Description
As part of InterSystems IRIS® data platform startup, UNIX® systems assign new processes a UNIX® PATH environment
variable. By default, the UNIX® PATH variable is:

PATH=/usr/bin:/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin

InterSystems IRIS processes that use PATH include system daemons, processes started by SYSTEM^%ZSTART, and processes the Super Server creates (such as JDBC/ODBC servers).

Customer applications may require that the UNIX® PATH environment variable for these processes have additional search directories appended to the default PATH provided by InterSystems IRIS. You can append directories to this path using the CPF Path variable. If you modify this setting, you must restart the instance for the change to take effect.

Note:

Terminal processes do not set their PATH this way; their PATH should be set by their login scripts.

Example

Path=/usr/customerapp/bin

This sets the PATH environment variable for system processes to:

PATH=/usr/bin:/bin:/usr/sbin:/usr/local/bin:/usr/local/sbin:/usr/customerapp/bin

On the Advanced Memory page of the Management Portal (System Administration > Configuration > Additional Settings > Advanced Memory), in the Path row, select Edit. Enter one or more directories, separated by colons.

Instead of using the Management Portal, you can change Path in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[config]

PythonPath

Specify directories to be added to sys.path for Embedded Python.

Synopsis

[config] PythonPath=directory

directory is one or more paths to valid directories. The maximum length for PythonPath is 1024 characters. By default, no directories are listed.

Description
When importing modules, Embedded Python uses the sys.path variable to determine the locations where Python packages may be located. PythonPath specifies additional directories where you plan to install Python packages, so that Embedded Python can find them when you w ant to import a module.

You may specify multiple directories. On Windows, separate multiple directories using a semicolon; on other platforms,
use a colon to separate multiple directories.

Example

On Ubuntu, PythonPath=/libraries/mypackages:/libraries/morepackages adds two directories to sys.path.

The example below, from Terminal, shows that the specified directories are present in sys.path when Embedded Python is launched.

USER>do ##class(%SYS.Python).Shell()

Python 3.10.13 (main, Aug 25 2023, 13:20:03) [GCC 9.4.0] on linux Type quit() or Ctrl-D to exit this shell. >>> import sys >>> sys.path ['/usr/lib/python310.zip', '/usr/lib/python3.10', '/usr/lib/python3.10/lib-dynload', '/InterSystems/IRIS/lib/python', '/InterSystems/IRIS/mgr/python', '/libraries/mypackages', '/libraries/morepackages', '/bin/local/lib/python3.10/dist-packages', '/bin/lib/python3/dist-packages',

'/bin/lib/python3.10/dist-packages']

On the Advanced Memory page of the Management Portal (System Administration > Configuration > Additional Settings > Advanced Memory), in the PythonPath row, select Edit. Enter one or more directories.

Instead of using the Management Portal, you can change PythonPath in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

When you edit this setting, it takes effect immediately. The new path is used for any new processes.

PythonRuntimeLibrary

PythonRuntimeLibrary

Specify the location of the runtime library to use when running Embedded Python.

Synopsis

[config] PythonRuntimeLibrary=library

library is the absolute path and filename of a Python runtime library . The maximum length for PythonRuntimeLibrary is 1024 characters. By default, no library is listed.

Description
PythonRuntimeLibrary specifies the location of the Python runtime library to use when running Embedded Python.

This location will vary based on your operating system, Python version, and other factors.

Windows example: PythonRuntimeLibrary=C:\Program Files\Python311\python3.dll (Python 3.11 on
Windows)

Linux example: PythonRuntimeLibrary=/usr/lib/x86_64-linux-gnu/libpython3.11.so.1.0 (Python
### 3.11 on Ubuntu 22.04 on the x86 architecture)

This parameter is required in order to use Embedded Python on InterSystems IRIS 2024.2 and higher on Windows. This parameter is needed on other platforms only when overriding the default version of Python.

Note:

See Flexible Python Runtime Feature for more information on configuring the v ersion of Python to be used for
Embedded Python.

See Other Supported Features for information on which platforms Flexible Python Runtime is available.

On the Advanced Memory page of the Management Portal (System Administration > Configuration > Additional Settings > Advanced Memory), in the PythonRuntimeLibrary row, select Edit. Enter the location of the Python runtime library you want to use.

Instead of using the Management Portal, you can change PythonRuntimeLibrary in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

When you edit this setting, it takes effect immediately. The new runtime library is used for any new processes.

[config]

PythonRuntimeLibraryVersion

Specify the version number of the runtime library to use when running Embedded Python.

Synopsis

[config] PythonRuntimeLibraryVersion=version

version is the version number of the Python runtime library to be used for Embedded Python. By default, no version is listed.

Description
PythonRuntimeLibraryVersion specifies the v ersion number of the Python runtime library to use when running Embedded Python. Use the major version only.

For example: PythonRuntimeLibraryVersion=3.11 (not 3.11.x)

This parameter is required in order to use Embedded Python on InterSystems IRIS 2024.2 and higher on Windows. This parameter is recommended on all other platforms.

Note:

See Flexible Python Runtime Feature for more information on configuring the v ersion of Python to be used for
Embedded Python.

See Other Supported Features for information on which platforms Flexible Python Runtime is available.

On the Advanced Memory page of the Management Portal (System Administration > Configuration > Additional Settings > Advanced Memory), in the PythonRuntimeLibraryVersion row, select Edit. Enter the version number of the Python runtime library you want to use.

Instead of using the Management Portal, you can change PythonRuntimeLibraryVersion in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

When you edit this setting, it takes effect immediately. The new runtime library is used for any new processes.

UUIDv1RandomMac

UUIDv1RandomMac

Controls how MAC addresses are treated within the UUID infrastructure.

Synopsis

[config] UUIDv1RandomMac=1

Allowed values are 0 and 1.

Description
This parameter controls how MAC addresses are treated when generating GUIDs. In particular, if this parameter is 1, that causes a MAC address to be represented by 47 bits of randomness and one high multicast bit as per RFC4122, Section 4.5. InterSystems suggests that you use UUIDv1RandomMac=1 if you are generating GUIDs where there are multiple instances on one host.

For details, see the CreateGuid() method of %SYSTEM.Util.

On the Advanced Memory page of the Management Portal (System Administration > Configuration > Additional Settings > Advanced Memory), in the UUIDv1RandomMac row, select Edit. Enter 0 or 1 and click Save.

Instead of using the Management Portal, you can change UUIDv1RandomMac in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

When you edit this setting, it takes effect immediately.

[config]

bbsiz

Set the maximum memory per process.

Synopsis

[config] bbsiz=n

n is an integer in the range 256—2,147,483,647 (KB). The default value is -1, which sets the parameter to its maximum value (2,147,483,647 KB).

Description
bbsiz is the maximum memory allocation permitted for a process (in kilobytes). This amount of process private memory is used for symbol table allocation and various other memory requirements, including I/O devices. It is allocated in increasing extents as required by the application until the maximum is reached.

Once this memory is allocated to the process, it is generally not deallocated until the process exits. However, when a large amount of memory is used (for example greater than 32MB) and then freed, InterSystems IRIS® data platform attempts to release deallocated memory back to the operating system where possible.

On the Memory and Startup page of the Management Portal (System Administration > Configuration > System Configuration > Memory and Startup), enter a number of kilobytes in the Maximum Per-Process Memory (KB) row.

Instead of using the Management Portal, you can change bbsiz in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

When you edit this setting, the updated value applies for all new processes.

When set to the default of -1, or the maximum of about 2 terabytes, maximum size of per process memory is essentially unlimited, as it is very unlikely that a process would approach the use of that much memory.

console

console

Set the path to the messages log file (messages.log).

Synopsis

[config] console=VMSConsoleTerminal,ConsoleFile

ConsoleFile is the full path to the messages.log file. The maximum length for ConsoleFile is 227 characters. By default, no directory is listed.

Description
console contains two comma-separated values that configure the messages.log file, as described belo w:

ConsoleFile

The path to the messages.log file, where InterSystems IRIS® data platform logs messages. Change ConsoleFile to
specify a new location for the log file; ho wever, the name of the log file must al ways be messages.log. If no value is
specified, InterSystems IRIS writes to

install-dir/mgr/messages.log.

ConsoleFile is the second comma-separated value of the console parameter.

You can view the messages log on Messages Log page of the Management Portal (System Operation > System Logs > Messages Log). To further configure the messages.log file, see MaxConsoleLogSize parameter.

VMSConsoleTerminal

Not in use.

On the Advanced Memory page of the Management Portal (System Administration > Configuration > Additional Settings > Advanced Memory), in the ConsoleFile row, select Edit. Enter a directory path.

Instead of using the Management Portal, you can change ConsoleFile in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF ).

[config]

errlog

Set the maximum number of entries in the error log.

Synopsis

[config] errlog=n

n is an integer in the range 10—10,000. The default value is 500.

Description
errlog is the maximum number of entries in the InterSystems IRIS® data platform system error log (see InterSystems IRIS System Error Log for more information). The log file e xpires old entries as this limit is reached.

On the Advanced Memory page of the Management Portal (System Administration > Configuration > Additional Settings > Advanced Memory), in the errlog row, select Edit. Enter a number of entries.

Instead of using the Management Portal, you can change errlog in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

globals

globals

Allocate shared memory to the database cache.

Synopsis

[config] globals=0,0,c,d,e,f

Description
globals contains six comma-separated values that specify how much shared memory to allocate to the database cache
for each block size. From left to right, each value is the number of megabytes allocated for:

- a – no longer used, always 0

- b – no longer used, always 0

- c – 8–kilobyte blocks

- d – 16–kilobyte blocks

- e – 32–kilobyte blocks

- f – 64–kilobyte blocks When all six values are set to 0, as they are by default, the system automatically allocates 25% of total physical memory. On a 64–bit system, there is a limit of 16 TB.

To create a database with a block size other than the default 8-KB blocks, you must enable additional block sizes using the DBSizesAllowed parameter in the [Startup] section.

For more information about allocating memory to the database cache, including using the Management Portal to do so, see Allocating Memory to the Database and Routine Caches.

You can also change globals using the Config.config class or by

editing the CPF.

- The routines parameter

- The DBSizesAllowed parameter in the [Startup] section of this reference

- Shared Memory Allocations.

- Allocating Memory to the Routine and Database Caches

- Large Block Size Considerations

- ECP Control Structures [config] gmheap

Set the size of the shared memory heap.

Synopsis

[config] gmheap=n

n is an integer in the range 16384—1,073,741,760 (KB). The default is 0 which will set the size of the shared memory heap to a reasonable value based on the size of the system.

Description
gmheap is the size in kilobytes of the shared memory heap (formerly known as the generic memory heap) for InterSystems IRIS® data platform. Shared memory is allocated from this total as needed for particular purposes (such as global mapping, database name and directory information, the security system, and so on). The shared memory in use by a given subsystem at a given time may be less than what is currently allocated.

Shared memory allocation is shown on the Shared Memory Heap Usage page (System Operation > System Usage, then click
the Shared Memory Heap Usage button); see Shared Memory Heap Usage for more information. Although this page displays
memory allocation and use in bytes, bear in mind that shared memory is allocated in pages.

By default gmheap is set to 0. This allows the system to select a reasonable value based on the overall system size. If 0 is selected, the system will configure the size of gmheap to be 3% of the total memory configured for global buffers. The minimum gmheap will be configured to is 307,200 KB (300 MB) and the maximum is 2,097,000 KB (2 GB), ho wever you can still manually configure a lar ger or smaller amount of memory.

Under some circumstances it may be necessary to increase gmheap to make enough shared memory available, for example
in the following situations:

- Restoring journal files To ensure optimal performance during a journal restore, InterSystems recommends that you increase the shared
memory heap size; see System Requirements for Parallel Dejournaling for more information.

- When parallel SQL query execution is in use Parallel query execution uses additional shared memory from the shared memory heap, and an increase in gmheap may therefore be required to optimize parallel query performance. See Shared Memory Considerations for more information.

- Creating a large number of interoperability-enabled namespaces When an interoperability-enabled namespace is created, many mappings are automatically defined for this namespace, see Create an Interoperability-Enabled Namespace. To support these mappings, additional shared memory from the shared memory heap is used, and an increase in gmheap may therefore be required to ensure enough shared memory is available.

The locksiz setting configures the portion of total a vailable shared memory that can be specifically allocated for managing locks (the lock table). locksiz is a subset of gmheap, and the remainder of gmheap is what is available for all other subsystems, so it is important that gmheap and locksiz be sized in consideration of this relationship, and that when locksiz is increased, gmheap is also increased proportionally.

On the Advanced Memory page of the Management Portal (System Administration > Configuration > Additional Settings > Advanced Memory), in the gmheap row, select Edit. Enter a number of kilobytes.

Instead of using the Management Portal, you can change gmheap in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

If you edit the gmheap setting, you must restart InterSystems IRIS to apply the change. You may also need to increase the allocated huge or large pages when increasing gmheap.

gmheap

[config]

history

Define command line recall options.

Synopsis

[config] history=n

n is an integer in the range 0—1000. The default value is 500.

Description
history is the maximum number of entries held in the command line/read line recall buffer.

On the Advanced Memory page of the Management Portal (System Administration > Configuration > Additional Settings > Advanced Memory), edit the values for history.

Instead of using the Management Portal, you can change history in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

This setting does not require a restart to take effect.

ijcbuff

ijcbuff

Set the size of an InterJob Communication buffer.

Synopsis

[config] ijcbuff=n

n is an integer in the range 512—8192 (bytes). InterSystems recommends you use the default of 512.

Description
ijcbuff is the number of bytes allocated for each InterJob Communication (IJC) buffer. For details, see Interprocess Communication. Also see the parameter ijcnum.

On the Advanced Memory page of the Management Portal (System Administration > Configuration > Additional Settings > Advanced Memory), in the ijcbuff row, select Edit. Enter a number of bytes.

Instead of using the Management Portal, you can change ijcbuff in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

If you edit this setting, you must restart InterSystems IRIS® data platform to apply the change.

[config]

ijcnum

Set the number of InterJob Communication devices.

Synopsis

[config] ijcnum=n

n is an integer in the range 0—256. InterSystems recommends you use the default of 16 devices.

Description
ijcnum is the number of InterJob Communication (IJC) devices. Each device corresponds to one InterJob Communication buffer of the size defined by ijcbuff. For details, see Interprocess Communication.

On the Advanced Memory page of the Management Portal (System Administration > Configuration > Additional Settings > Advanced Memory), in the ijcnum row, select Edit. Enter a number of devices.

Instead of using the Management Portal, you can change ijcnum in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

If you edit this setting, you must restart InterSystems IRIS® data platform to apply the change.

jrnbufs

jrnbufs

Allocate memory for journal buffers.

Synopsis

[config] jrnbufs=n

n is an integer. The maximum value is 1024 (MB), and the minimum is 16 for Unicode instances and 8 for 8-bit instances. The default value is 64.

Description
jrnbufs is the amount of memory allocated for journal buffers. Increasing this setting means increasing the amount of journal data that can be held in memory, which improves journaling performance but increases the amount of journal data that could potentially be lost in the event of a system failure.

Increasing jrnbufs also potentially means reducing the journal file size (as configured in the Management Portal, using
^JRNOPTS, or by setting the FileSizeLimit parameter) due a combined total size limit of 4 GB for the two settings;
see Configuring Journal Settings for more information.

On the Advanced Memory page of the Management Portal (System Administration > Configuration > Additional Settings > Advanced Memory), in the jrnbufs row, select Edit. Enter a number of megabytes.

Instead of using the Management Portal, you can change jrnbufs in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

If you edit this setting, you must restart InterSystems IRIS® data platform to apply the change.

[config]

locksiz

Set the maximum size of shared memory for locks.

Synopsis

[config] locksiz=n

n is an integer with a minimum value of 65,536 (bytes). The default value is 0 which is designed to be appropriate for all systems. If locksiz is set to 0, it will be limited only by the size shared memory given by the gmheap parameter.

Description

The default value of locksiz (n=0) is designed to be appropriate for all systems. In this case, the lock table can grow
unlimited up to the available space in gmheap (the Shared Memory Heap); since memory used to allocate locks is taken
from gmheap , you cannot use more memory for locks than is available in gmheap. If you need more room for the lock table, increase the gmheap parameter as well. Alternatively, you can lower the LockThreshold to use less space in the lock table.

If locksiz is set to a nonzero value, this places a fix ed upper bound on the amount of shared memory that is allocated for locks. However, it is recommended to use the default value of locksiz=0 unless you have application code that relies on a fix ed upper bound.

If you edit this setting, changes take effect immediately.

On the Advanced Memory page of the Management Portal (System Administration > Configuration > Additional Settings > Advanced Memory), in the locksiz row, select Edit. Enter a number of bytes.

Instead of using the Management Portal, you can change locksiz in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

- Locking and Concurrency Control

- The gmheap parameter

- The LockThreshold parameter memlock memlock

Specify locking shared memory or the text segment into memory.

Synopsis

[config] memlock=n

n is a set of bit flags. By def ault, the n is 0 (all flags are set to f alse).

Description
memlock is a set of bit flags that controls ho w InterSystems IRIS® data platform allocates shared memory. At startup, InterSystems IRIS allocates a shared memory segment for use by control structures, global buffers, routine buffers, and shared memory heap. The memlock parameter offers detailed control over how that allocation occurs.

By default (n = 0), InterSystems IRIS attempts to allocated shared memory from large pages on platforms that support
large pages (Windows, Linux, and AIX) as follows:

1. Request large pages, if allowed. Large pages are automatically locked in physical memory at the operating system

level.

2.

3.

If unable to allocate the full amount of configured memory in lar ge pages, request standard (small) pages. InterSystems IRIS does not attempt to lock standard pages into physical memory.

If unable to allocate the full amount of configured memory in small pages, reduce the allocation by one eighth (1/8) and begin again with step 1.

The following bit flags modify this process as described belo w:

## 1 (LockSharedMemory)

This memlock flag indicates whether shared memory is lock ed in physical memory when large pages are not being used. By default, it is not. This applies to all operating systems except for Microsoft Windows and macOS.

## 8 (LockTextSegment)

This memlock flag indicates whether the te xt segment (the InterSystems IRIS executable code space) is locked in physical memory (on some UNIX platforms). By default, it is not.

## 32 (LargePagesDisabled)

This memlock flag indicates whether to disable lar ge/huge pages for shared memory on platforms that support them. By default, large/huge pages are used.

When this flag is of f on platforms supporting large pages, InterSystems IRIS attempts to allocate memory in large pages and switches back to standard pages if large pages cannot be allocated at the requested size. Technically, this means that InterSystems IRIS adopts a neutral disposition towards page size, taking no action to request large pages.

## 64 (LargePagesRequired)

This memlock flag indicates whether to require use of lar ge/huge pages for shared memory on platforms that support them (Windows, AIX, and Linux). By default, it is not required. This flag is ignored on other platforms, or if lar ge pages are disabled by the LargePageDisabled flag.

When LargePagesRequired is True (and not ignored), if memory cannot be allocated in large/huge pages, startup is aborted rather than falling back to small pages. InterSystems IRIS retries with a small reduction in memory size, but not as much of a reduction as could occur in absence of this flag.

[config]

## 128 (BackoffDisabled)

This memlock flag indicates, on f ailure to allocate memory, whether to retry with a reduced amount. By default, it does retry. If this flag is True and memory cannot be allocated at its configured size, startup is aborted.

On the Advanced Memory page of the Management Portal (System Administration > Configuration > Additional Settings > Advanced Memory), choose true or false for BackoffDisabled, LargePagesDisabled, LargePagesRequired, LockSharedMemory, and LockTextSegment.

Instead of using the Management Portal, you can change memlock by changing the values of BackoffDisabled, LargePagesDisabled, LargePagesRequired, LockSharedMemory, and LockTextSegment in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

If you edit this setting, you must restart InterSystems IRIS to apply the change.

netjob

netjob

Allow remote job requests.

Synopsis

[config] netjob=n

n is either 1 (true) or 0 (false). The default value is 1.

Description
When netjob is enabled (n = 1), incoming remote job requests via ECP are honored on this server.

On the Advanced Memory page of the Management Portal (System Administration > Configuration > Additional Settings > Advanced Memory), in the netjob row, select Edit. Choose true or false.

Instead of using the Management Portal, you can change netjob in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[config]

nlstab

Set the number of NLS collation tables.

Synopsis

[config] nlstab=n

n is an integer in the range 0—64. The default value is 50.

Description
nlstab is the number of NLS collation tables for which to allocate when InterSystems IRIS® data platform starts up. This parameter refers to loadable national collation tables and does not include built-in collations.

On the Advanced Memory page of the Management Portal (System Administration > Configuration > Additional Settings > Advanced Memory), in the nlstab row, select Edit. Enter a number of tables.

Instead of using the Management Portal, you can change nlstab in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

overview

overview

Review instance platform and version summary.

Synopsis

[config] overview=a~b

a and b are read-only strings.

Description
overview displays two tilde-separated (~) values which provide platform and version information. From left to right:

- a: The specific operating system on which InterSystems IRIS® data platform is running.

- b: The general type of the operating system.

Examples

overview=Windows(Intel)~Windows

overview=Linux (Intel)~UNIX®

[config]

pijdir

Not in use.

Description
Not in use.

routines

routines

Allocate shared memory to the routines cache

Synopsis

[config] routines=n ; This is the format for single-value automatic allocation.
[config] routines=n1, n2, n3, n4, n5, n6 ; This is the format for multiple-value manual allocation.

The default for this parameter is routines=0. This allocates memory equal to 10% of the memory allocated to 8-kilobyte global buffers in the database cache by the globals parameter, with a minimum of 80 MB and a maximum of 1 GB.

Description
The routines parameter specifies the amount of memory to allocate for b uffering routines. The total memory is shared across six pools of different buffer sizes, from 2-kilobyte to 64-kilobyte,

You can configure routine b uffers pools in one of two ways — either specify a single value for automatic allocation, or
multiple values to allocate buffers manually. For typical production instances, automatic allocation is sufficient; ho wever,
the ideal allocation for a given application depends on many factors, and adjustment may be necessary to optimize performance.

Automatic (Single-value) Buffer Allocation

You can specify a single value, n, as the total number of megabytes that InterSystems IRIS® data platform allocates for
routine buffers. The minimum size is 80 MB; if a smaller value is specified, the instance adjusts up to 80 MB. A value of
0 (the default) allocates memory equal to 10% of the memory allocated to 8-kilobyte global buffers in the database cache by the globals parameter, with a minimum of 80 MB and a maximum of 1020 MB.

InterSystems IRIS divides the memory among the 4, 16, and 64-kilobyte buffer pools as follows:

- 12.5% in 4-kilobyte buffers

- 37.5% in 16-kilobyte buffers

- 50% in 64-kilobyte buffers

For example, if you specify routines=500, InterSystems IRIS creates:

- 16,000 4-kilobyte buffers (62.5 MB)

- 12,000 16-kilobyte buffers (187.5 MB)

- 4,000 64-kilobyte buffers (250 MB) Manual (Multi-value) Buffer Allocation

You can specify the amount of memory to allocate for each routine buffer pool. If you do, you must specify all six values;
if fewer than six are given, InterSystems IRIS reverts to the automatic format described above, using the first v alue provided.
The six values represent the following:

- n1 is the number of MB allocated for 2-kilobyte routine buffers.

- n2 is the number of MB allocated for 4-kilobyte routine buffers.

- n3 is the number of MB allocated for 8-kilobyte routine buffers.

- n4 is the number of MB allocated for 16-kilobyte routine buffers.

- n5 is the number of MB allocated for 32-kilobyte routine buffers.

[config]

- n6 is the number of MB allocated for 64-kilobyte routine buffers. The instance always allocates at least 430 64 KB routine buffers, regardless of the value of n6.

For example, if you specify routines=0,128,128,0,0,800, InterSystems IRIS creates:

- 128 MB of 4-kilobyte buffers

- 128 MB of 8-kilobyte buffers

- 800 MB of 64-kilobyte buffers

While it is possible to allocate 0 buffers of a particular size (except for 64 KB), the next minimum value is 430; if a smaller
number is specified, the instance allocates 430. The combined maximum number of buffers is 33,554,432. The format for InterSystems IRIS routines does not allow more than 32,768 characters for literal strings regardless of the setting for the maximum routine size.

For more information about allocating memory to the routine cache, including using the Management Portal to do so, see Allocating Memory to the Database and Routine Caches.

You can also change routines using the Config.config class or by

editing the CPF.

- The globals parameter.

- Shared Memory Allocations

- Allocating Memory to the Routine and Database Caches udevtabsiz udevtabsiz

Set the maximum size of the device table.

Synopsis

[config] udevtabsiz=n

n is an integer in the range 0—65,535 (bytes). The default value is 24,576.

Description
udevtabsiz is the maximum size in bytes of the device table. This table maps device numbers (traditional logical unit numbers) to device names, so that ObjectScript code can open devices by number.

On the page System Administration > Configuration > Additional Settings > Advanced Memory, in the udevtabsiz row, select Edit. Enter a number of bytes.

Instead of using the Management Portal, you can change udevtabsiz in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[config]

wijdir

Set the Write Image Journal files directory .

Synopsis

[config] wijdir=n

n is the full path to a valid directory. The maximum length is 226 characters. By default, no directory is listed.

Description
wijdir is the name of the directory where the write image journal file is stored. InterSystems recommends that the directory which stores the write image journal be located in a different partition from your databases.

If no value is specified, InterSystems IRIS® data platform uses the

install-dir/mgr directory.

If an instance is a member of a cluster, activating a Write Image Journal directory change requires a system restart.

On the Journal Settings page of the Management Portal (System Administration > Configuration > System Configuration > Journal Settings), in the Write image journal directory row, select Browse. Select a directory name.

Instead of using the Management Portal, you can change wijdir in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

targwijsz

targwijsz

Set the desired size of the wij.

Synopsis

[config] targwijsz=n

n is an integer. The default is 0.

Description
targwijsz allocates a number of MB for the Write Image Journal (WIJ) file. The value is an integer, but fractional input is accepted and silently truncated (for example, 35.5 becomes 35).

When targwijsz is set to 0, the WIJ grows as needed, based on activity. The targwijsz parameter allows you to allocate space to the WIJ ahead of time so it does not need to grow during a period of high activity. While there is no upper limit for targwijsz, it is not useful to set it higher than the maximum size of the database cache, as that is the most space the WIJ needs.

For more information, see Write Image Journaling and Recovery.

Note:

Setting this target size ensures that disk space is allocated for the WIJ early in the start-up process. If sufficient space is not allocated early and there is not enough available space for the WIJ, the instance may encounter problems. Allocating space for WIJ is an advanced configuration setting. If you encounter issues with this, contact the InterSystems Worldwide Response Center.

On the Advanced Memory Settings page of the Management Portal (System Administration > Configuration > Additional Settings > Advanced Memory), click the Edit link in the targwijsz row. The Edit: targwijsz page provides details about the setting and allows you to change its value.

Instead of using the Management Portal, you can change targwijsz in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[config]

zfheap

Define the size and configuration of the $ZF heap.

Synopsis

[config] zfheap=ZFString,ZFSize

ZFString is an integer in the range 0—32,767. The default is 0.

ZFSize is an integer in the range 0—270,336 (bytes). The default is 0.

Description
zfheap contains two comma-separated values which configure the $ZF heap: ZFString and ZFSize. For more details about
the $ZF heap, see Creating a Callout Library.

ZFString

ZFString is the minimum number of characters that are reserved in the $ZF heap for output strings other than IRIS_EXSTR
strings. When set to 0, InterSystems IRIS® data platform uses the system default value of 32,767. If an output string has an initial value larger than ZFString, that larger value is the reserved space. An application must not return a string longer than this reserved space. The value of ZFString is not relevant for input strings.

ZFSize

ZFSize is the number of bytes InterSystems IRIS allocates for the $ZF heap for all purposes. When set to 0, InterSystems
IRIS uses the system default value of 135,168. Each input string is copied into the $ZF heap. Each output string allocates
the larger of the current length of the string and ZFString. Note that for a Unicode instance of InterSystems IRIS, each
character of a string uses 2 bytes of $ZF heap space. IRIS_EXSTR strings do not use the $ZF heap.

On the Advanced Memory page of the Management Portal (System Administration > Configuration > Additional Settings > Advanced Memory), edit the values for ZFString or ZFSize. InterSystems recommends that this parameter be set to 0,0.

Instead of using the Management Portal, you can change zfheap in the Config.config class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

If you edit this setting, the updated value is effective for new InterSystems IRIS processes.

[ConfigFile]

This topic describes the parameters found in the [ConfigFile] section of the CPF .

[ConfigFile]

Version

Review the version of the configuration parameter file.

Synopsis

[ConfigFile] Version=n

n is a string of numbers up to 7 characters long, usually in dot format.

Description
Version is the configuration parameter file (CPF) v number. It is updated automatically during product upgrade or installation. The number refers to the syntax and structure of the parameter file, and not to the changes that you might mak e to the parameter values within the file.

ersion number. This value is independent of the product release

Important:

Changing this value from the default can cause InterSystems IRIS® data platform to fail on startup. To differentiate between multiple user-edited CPFs, use Comments as described in “Introduction to the Configuration P arameter File”.

[Databases]

This topic describes the parameters found in the [Databases] section of the CPF.

[Databases]

Database

Define InterSystems IRIS® data platform databases.

Synopsis

[Databases] Name=a,b,c,d,e,f

Name is a string. a, b, c, d, e, and f are as described below.

Description
The [Databases] section of the configuration parameter (CPF) file contains an entry for e the instance. Each entry has up to six comma-separated arguments that define a database. The only argument that is required is the first, the directory specification. If the other ar

guments are not specified, then the def ault is used. The values are:

very database connected to

- a — Full directory specification for database file. Required.

- b — Remote instance name (empty for local instance). If the instance is remote (nonempty name), subsequent fields are ignored. Default is the local instance.

- c — Must mount at startup (1 or 0). When enabled, the database must be mounted at startup, or startup fails. Default is 0 (database is not mounted at startup).

- d — This value is ignored and may be left blank.

- e — This value is ignored and may be left blank.

- f — Stream Location. Directory where the streams associated with this database go. The default is blank — "" (which InterSystems recommends.) This default location is in the subdirectory stream, underneath the database directory. For example, for a database located in c:\abc, the default stream location is c:\abc\stream.

By default, even if they are not specified, the IRISSYS, IRISLIB, IRISTEMP , and IRISAUDIT databases must be local to the instance and mounted at startup.

Examples

In the [Databases] section, each line is a database with arguments Name=a,b,c,d,e,f. Here is an example from Windows:

[Databases]
IRISSYS=c:\InterSystems\IRIS\mgr\
IRISLIB=c:\InterSystems\IRIS\mgr\irislib\ IRISTEMP=c:\InterSystems\IRIS\mgr\iristemp\ IRISAUDIT=c:\InterSystems\IRIS\mgr\irisaudit\ USER=c:\InterSystems\IRIS\mgr\user\ SALES=c:\sales\,SALESERVER
; Database is on instance SALESERVER
BILLING=/usr/billing/,,1
; Database is local and mount required

Example of remote databases without mirroring:

PRDAUDIT=c:\InterSystems\IRIS\mgr\prdaudit\,PRD
PRDDCIFC=\InterSystems\IRIS\mgr\prddata\,PRD
PRDERR=\InterSystems\IRIS\mgr\prderr\,,1

Example of remote databases with mirroring. Note in this mirrored example the two remote databases, both mirrored and nonmirrored, are formatted differently than they would be in a nonmirrored environment.

PRDAUDIT=:mirror:PRDMIRROR:PRDAUDIT,PRD - Mirrored remote database PRDDCIFC=:ds:PRDDCIFC,PRD - Nonmirrored remote database PRDERR=\InterSystems\IRIS\mgr\prderr\,,1 - Local database

On the Local Databases page of the Management Portal (System Administration > Configuration > System Configuration > Local Databases), to add a new entry, select Create New Database. To edit an existing entry, select Edit in that entry's row.

Database

[Debug]

ug] section. The [Debug] settings can be used for different kind of The configuration parameter file may include a [Deb diagnostics. This topic describes two parameters that may be found in the [Debug] section of the configuration parameter file. All other [Debug] settings are InterSystems proprietary and this book does not document them.

[Debug]

Dumpstyle

Specify the style of core dump.

Synopsis

[Debug] dumpstyle=n

n is an integer in the range 0—4. The default value is 3.

Description
When InterSystems IRIS® data platform performs a core dump, you can set the style of the dump using this option. Values and their meanings are listed in the following.

Note:

On UNIX®, all dump styles generate a core file. The process cleans itself up as much as possible before exiting.

- – On Windows this is the pid.dmp file.

- – On Windows this is a Windows minidump file (type = MiniDumpWithFullMemory) named irisfpid.dmp, which can be read by WinDbg, a Microsoft debugger. This is the most complete dump option, but it can create a huge dump file.

- – On Windows this is the old style exception processing where the process does minimal cleanup (deqallresources and GRETRELEASE) and then resignals the exception. The intention here is to catch the exception in a debugger and preserve as much information as possible to analyze.

– On UNIX® this detaches shared memory before the Abort, so the core file does not contain the shared memory

area.

- On Windows this is a Windows minidump file (type = MiniDumpWithDataSegs | MiniDumpWithPrivateReadWriteMemory | MiniDumpWithIndirectlyReferencedMemory) named irisipid.dmp, which can be read by WinDbg. This creates a fairly large but useful dump file. This is the new default on Windows if dumpstyle is not specified.

- On Windows this is a Windows minidump file (type = MiniDumpNormal) named irismpid.dmp, which can be read by WinDbg. This creates a small dump file containing minimal information.

The active value can be changed with $system.Config.ModifyDumpStyle(NewValue). This changes the value for all new
InterSystems IRIS processes. It does not change the value in iris.cpf.

You can change Dumpstyle by editing the CPF in a text editor (as described in Editing the Active CPF). If the CPF does
not already contain the [Debug] section, you must add it yourself. For example:

Dumpstyle

[ConfigFile]
...

[Debug] // Add the debug section if necesary dumpstyle=2 // then, specify a value for dumpstyle.

[Databases]
...

[Debug]

Semsperset

Set number of semaphores allocated per set.

Synopsis

[Debug] semsperset=n

n is an integer. There is no maximum value. The default value is 0.

Description
Semsperset is the number of semaphores InterSystems IRIS® data platform should allocate per semaphore set. When this parameter is set to 0 (the default), InterSystems IRIS allocates the maximum number of semaphores to each set, which results in the minimum number of semaphore sets.

A larger value for semsperset results in fewer semaphore sets of a larger size. There may be a performance benefit to having fewer semaphores per set, particularly on Linux systems (for example, by setting semsperset equal to 250), although this has not been thoroughly tested.

For more information, see Semaphores in InterSystems Products.

You can change semsperset by editing the CPF in a text editor (as described in Editing the Active CPF). If the CPF does
not already contain the [Debug] section, you must add it yourself. For example:

[ConfigFile]
...

[Debug] // Add the debug section if necesary, semsperset=250 // then specify a value for semsperset.

[Databases]
...

[Devices]

This topic describes the parameters found in the [Devices] section of the CPF.

[Devices]

Devices

Define de vice types.

Synopsis

[Devices] Name=a^b^c^d^e^f^g^h

Description
The [Devices] section of the configuration parameter file (CPF) contains an entry for e IRIS® data platform. Each entry has a Name, which is the defined de vice title or number, and eight strings separated by up-arrows (^) that define a de vice. The maximum length of all strings is 128 characters, except for the Description (g) string, which can be up to 256 characters. The entries are as follows

very device detected by InterSystems

- (a) PhysicalDevice— The physical name used to refer to the device. The PhysicalDevice value specifies the device
argument for this device’s OPEN command. The name can contain up to 128 alphanumeric characters; it can contain
space characters as well. For example, for a printer you could enter the following, where MYNAME is the computer name.

- |PRN|\\MYNAME\ISF-HP5SiMX7

- Or:

- |PRN|\\MYNAME\Canon PIXMA

- (b) Type— The type of device. Options: TRM=Terminal. SPL=Spooling device. MT=Magnetic Tape drive. BT=Cartridge tape drive. OTH=Any other device including printers and sequential files. The default depends on the device type.

(c) SubType— Used to refine the definition of your de vice subtypes. Subtypes specify terminal characteristics. They are used to create the appropriate OPEN command for the device. There should be subtype information for every terminal type.

(d) Prompt— Choose a prompt: valid inputs are 1, 2, or NULL (a blank or empty field). 1 corresponds to Auto-use this device if it is the current device. 2 corresponds to Auto-use this device with predefined settings (predefined Right Margin and Parameter settings). NULL corresponds to Show device prompt (the user sees the device selection prompt with the default device defined).

(e) OpenParameter— A colon-separated string that provides the parameters, timeout, and mnespace arguments for
this device’s OPEN command. The syntax for the OpenParameter string is:

(parameters):timeout:"mnespace"

Inside the parentheses for parameters, individual items are colon-separated, as follows:

param1:param2:param3

Resulting in:

(param1:param2:param3):timeout:"mnespace"

timeout and mnespace are optional, but if they are provided, the correct number of colons must separate them from previous entries in the OpenParameter string.

parameters must be contained within parentheses only if there is more than one parameter. If there are no parameters, or if there is only one parameter, the parentheses may be omitted from the string. Thus the following is a correct and
complete OpenParameter string:

:timeout:"mnespace"

If provided, mnespace must be contained within double quotes, as shown.

Devices

For details about the OPEN command and its arguments, including a large variety of syntax examples, see the
ObjectScript Reference.

(f) AlternateDevice— The device ID of another device. The value entered for AlternateDevice must be a defined mnemonic such as the Name supplied for another device.

Specifying an AlternateDevice value for the device allows users of the %IS utility to specify “A” to tell InterSystems IRIS to use the alternate device. %IS is a general device selection utility for character-based applications. For details about %IS see Allowing Users to Specify a Device, especially the section %IS Mnemonics, which describes the conventions for entering the “ A” code for %IS.

(g) Description— A text description of where the device is located. This field is for your o wn reference to help you identify what machine you are configuring.

(h) Alias— An alternate device ID (number) for this device. All aliases must be unique. You can use this value as the device argument in an OPEN command.

- Examples

- In the [Devices] section, each entry Name=a^b^c^d^e^f^g^h appears all on one line:

- [Devices]
0=0^TRM^C-Terminal^^^^Principal device^
2=2^SPL^PK-DEC^^^^Spool LA120^
SPOOL=2^SPL^PK-DEC^^^^Spool LA120^
TERM=0^TRM^C-Terminal^^^^Windows Console^
|PRN|=|PRN|^OTH^P-DEC^^"W"^^Windows Printer^
|TNT|=0^TRM^C-VT220^^^^Principal device^
|TRM|=0^TRM^C-Terminal^^^^Windows Console^

On the Devices page of the Management Portal (System Administration > Configuration > Device Settings > Devices) is a list of existing devices. Select Create New Device, Edit, or Delete to modify the list.

[DeviceSubTypes]

This topic describes the parameters found in the [DeviceSubTypes] section of the configuration parameter file.

[DeviceSubTypes]

DeviceSubTypes

Define de vice subtypes.

Synopsis

[DeviceSubTypes] Name=n=a^b^c^d^e^f^g^h^i

Description
The [DeviceSubTypes] section of the configuration parameter file (CPF) contains an entry for each subtype configured for this installation. Each entry has a Name, as well as nine values separated by up-arrows (^) that define a de vice subtype. The entries are as follows

- (a) RightMargin— The number that represents the location of the right margin. Device output wraps at that number of characters.

- (b) FormFeed— The ASCII code that represents a form feed on the selected device in the form #,$C(code1,code2...).
This setting is used by the InterSystems IRIS® data platform command-line utilities.

- (c) ScreenLength— The number of lines that comprise one screen or page for the device.

- (d) Backspace— The ASCII code that represents the backspace character on the selected device in the form $C(code1).
This setting is used by the InterSystems IRIS command-line utilities.

- (e) CursorControl— The ASCII code that represents the cursor on the selected device in the form $C(code1).

- (f) EraseEOL— The ASCII code that represents erasing the end of line characters on this device in the form
$C(code1,code2).

- (g) EraseEOF— The ASCII code that represents erasing the end of file character on the selected de vice in the form
$C(code1,code2...).

- (h) ZU22FormFeed— The ASCII code that represents a form feed on the selected device in the form $C(code1,code2).
This setting is used by InterSystems Terminal output.

- (i) ZU22Backspace— The ASCII code that represents a backspace on the selected device in the form $C(code1). This
setting is used by InterSystems Terminal output.

Default values depend on the device type.

Examples

The following is a sample [DeviceSubTypes] section. This example wraps long lines to fit them onto the vie wing page. In
the .cpf file itself, each entry appears all on one line:

[DeviceSubTypes]
C-ANSI=80^#,$C(27,91,72,27,91,74)^25^$C(8)^W $C(27,91)_(DY+1)_";"_(DX+1)_"H"
S $X=DX,$Y=DY^$C(27,91,74)^$C(27,91,75)^$C(27,91,72,27,91,74)^$C(8,32,8)
C-Terminal=80^#,$C(27,91,72,27,91,74)^24^$C(8)^W $C(27,91)_(DY+1)_";"_
(DX+1)_"H" S $X=DX,$Y=DY^$C(27,91,74)^$C(27,91,75)^$C(27,91,72,27,91,74)^
$C(8,32,8)
C-TV925=80^#,$C(27,44)^24^$C(8)^W $C(27,61,DY+32,DX+32) S $X=DX,$Y=DY^^^
$C(27,44)^$C(8,32,8)
C-VT100=80^#,$C(27,91,72,27,91,74)^24^$C(8)^W $C(27,91)_(DY+1)_";"_(DX+1)_
"H" S $X=DX,$Y=DY^$C(27,91,74)^$C(27,91,75)^^
C-VT101W=132^#,$C(27,91,72,27,91,74)^14^$C(8)^W $C(27,91)_(DY+1)_";"_(DX+1)_
"H" S $X=DX,$Y=DY^$C(27,91,74)^$C(27,91,75)^^
C-VT132=132^#,$C(27,91,72,27,91,74)^24^$C(8)^W $C(27,91)_(DY+1)_";"_(DX+1)_
"H" S $X=DX,$Y=DY^$C(27,91,74)^$C(27,91,75)^^
C-VT220=80^#,$C(27,91,72,27,91,74)^24^$C(8)^W $C(27,91)_(DY+1)_";"_(DX+1)_"H"
S $X=DX,$Y=DY^$C(27,91,74)^$C(27,91,75)^$C(27,91,72,27,91,74)^$C(8,32,8)
C-VT240=80^#,$C(27,91,72,27,91,74)^24^$C(8)^W $C(27,91)_(DY+1)_";"_(DX+1)_"H"
S $X=DX,$Y=DY^$C(27,91,74)^$C(27,91,75)^$C(27,91,72,27,91,74)^$C(8,32,8)
C-VT52=80^#,$C(27,72)^24^$C(8)^W $C(27,89,DY+32,DX+32) S $X=DX,$Y=DY^^^^

DeviceSubTypes

M/UX=255^#^66^$C(8)^^^^^
MAIL=132^#^11^$C(8)^^^^^
P-DEC=132^#^66^$C(8)^^^^^
PK-DEC=150^#^66^$C(8)^^^^^
PK-QUME=150^#^66^$C(8)^^^^^

On the Device SubTypes page of the Management Portal (System Administration > Configuration > Device Settings > Device SubTypes) is a list of existing subtypes. Select Create New Sub Type, Edit, or Delete to modify the list.

[ECP]

This topic describes the parameters found in the [ECP] section of the CPF.

[ECP]

ClientReconnectDuration

Set duration for ECP reconnection attempt.

Synopsis

[ECP] ClientReconnectDuration=n

n is an integer in the range 10—65,636 (seconds). The default value is 1,200.

Description
ClientReconnectDuration is the number of seconds an Application Server (ECP client) should keep trying to reestablish a connection before giving up or declaring the connection failed. The Application Server (ECP client) continues reconnection attempts at intervals scheduled by the ClientReconnectInterval until the full ClientReconnectDuration expires. The default value 1200 is equivalent to 20 minutes.

On the ECP Settings page of the Management Portal (System Administration > Configuration > Connectivity > ECP Settings), in the This System as an ECP Application Server column, edit Time to wait for recovery.

Instead of using the Management Portal, you can change ClientReconnectDuration in the Config.ECP class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

ClientReconnectInterval

ClientReconnectInterval

Set interval between ECP reconnection attempts.

Synopsis

[ECP] ClientReconnectInterval=n

n is an integer in the range 1—60 (seconds). The default value is 5.

Description
ClientReconnectInterval is the number of seconds to wait between each reconnection attempt when a Data Server (ECP server) is not available. The Application Server (ECP client) continues reconnection attempts at intervals scheduled by ClientReconnectInterval until the full ClientReconnectDuration expires.

On the ECP Settings page of the Management Portal (System Administration > Configuration > Connectivity > ECP Settings) in the This System as an ECP Application Server column, edit Time between reconnections.

Instead of using the Management Portal, you can change ClientReconnectInterval in the Config.ECP class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[ECP]

ServerTroubleDuration

Set the number of seconds an ECP connection stays in troubled state.

Synopsis

[ECP] ServerTroubleDuration=n

n is an integer in the range 20—65,636 (seconds). The default value is 60.

Description
ServerTroubleDuration is the number of seconds an ECP connection stays in a troubled state. Once this period of time has elapsed, the Data Server (ECP server) declares the connection dead and presumes that recovery is not possible.

On the ECP Settings page of the Management Portal (System Administration > Configuration > Connectivity > ECP Settings), in the This System as an ECP Data Server column, edit Time interval for Troubled state.

Instead of using the Management Portal, you can change ServerTroubleDuration in the Config.ECP class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[ECPServers]

This topic describes the server entries found in the [ECPServers] section of the CPF.

[ECPServers]

ECPServers

Define ECP serv ers.

Synopsis

[ECPServers] Name=Address,Port,MirrorConnection,SSLConfig,BatchMode

Description
Entries in [ECPServers] define the address and port of the ECP serv er to connect to and the way in which connections should be redirected if the ECP server is a mirror primary. (See Configuring Application Server Connections to a Mirror for important information about configuring a mirror primary as a data serv er.)

- Address – Address of the ECP server to connect to.

- Port – Port # of the ECP server to connect to.

- MirrorConnection – Behavior when connecting to a mirror primary. Default is 0 (or blank), indicating that the data server is not a mirror member. A value of 1 indicates the ECP server is a mirror failover member, and the mirror connection redirects to whichever member is primary in the event of a failover. A value of -1 indicates the ECP server is
either a failover member or DR async, and the mirror connection is restricted to that specific ECP serv er; if the ECP
server becomes the backup member, it does not accept the connection until it becomes primary.

- SSLConfig – If 1, connections to the server use TLS/SSL.

- BatchMode – If 1, the server process for this data server runs in batch mode. In batch mode, the data server always loads blocks and caches them in batch level.

On the ECP Settings page of the Management Portal (System Administration > Configuration > Connectivity > ECP Settings) is a list of ECP data servers. Select Add Remote Data Server to add a new ECP data server.

Note:

You cannot set the MirrorConnection property to -1 from the Management Portal.

Instead of using the Management Portal, you can change ECPServers using the Config.ECPServ ers class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[Gateways]

This topic describes the [Gateways] section of the CPF, which contains entries for each gateway configured on the InterSystems IRIS® data platform instance and its host system. Gateways, which are also known as external language servers, provide fully integrated bi-directional connections between InterSystems IRIS and external language platforms.

Each definition consists of a name follo wed by = and then a comma-separated list of values defining the g ateway; some
fields are required, and others can be left blank, in which case def ault values are used. As shown in the following, there are seven types of gateways defined in the def ault CPF of a newly installed instance: .NET, IntegratedML, JDBC, Java, Python, R, and XSLT. In addition to these local gateway types, you can define a remote g ateway on a remote system.

[Gateways]
%DotNet Server=.NET,53398,%Gateway_Object,N6.0
%IntegratedML Server=ML,53598,%Gateway_ML
%JDBC Server=JDBC,53798,%Gateway_SQL
%Java Server=Java,53298,%Gateway_Object
%Python Server=Python,53498,%Gateway_Object
%R Server=R,53898,%Gateway_Object
%XSLT Server=XSLT,53698,%Gateway_Object

Note:

JDBC, Java, IntegratedML, XSLT, and R gateways are all Java-based and use the same fields in their definitions.

The Type and Port fields are required for all g ateway types. The third field, Resource, specifies the gateway resource a user
must have USE permission on to use the gateway; for example, the default %JDBC Server definition abo ve specifies that
a user must have the %Gateway_SQL:USE privilege to use the JDBC gateway. InterSystems strongly recommends protecting all gateways by specifying an appropriate resource.

If you have the %Admin_ExternalLanguageServerEdit:USE privilege, which by default is held by the %Manager
role, you can use the following methods to create a new gateway of any type or to edit or delete an existing gateway:

- The External Language Servers page of the Management Portal (System Administration > Configuration > Connectivity > External Language Servers).

- The Config.Gate way API.

- Modifying the CPF of the instance using the iris merge or by editing it directly.

You can start or start a gateway and display its activity log using the External Language Servers page or the $system.external
interface.

Once a gateway has been defined, either in the def ault CPF or by a user, its name cannot be changed.

[Gateways]

%DotNet Server

Define a .NET g ateway.

Synopsis

[Gateways]
%DotNet Server=.NET,Port,[optional fields]

Description
%DotNet Server is the name of the .NET gateway definition in the def ault CPF, but additional .NET gateways with different names can be created and existing gateways can be edited or deleted as described in [Gateways]. Each definition
consists of a name followed by = and then a comma-separated list of values defining the g ateway; some fields are required,
and others can be left blank, in which case default values are used. Fields specific to .NET g ateways and those that can be used with all gateways are described in the following two sections. For a more detailed description, see .NET External Server Setup and Defining External Serv er Configurations for .NET .

.NET Gateway Fields The first three fields in a .NET g
gateways. These are shown in order in the following:

ateway definition are common to all g ateways and the next three are specific to .NET

- Type (required) — Type of the gateway. When defining a .NET g ateway, this is set to .NET.

- Port (required) — TCP port number for communication between the gateway and the proxy classes in InterSystems IRIS. This port number must not conflict with an y other local TCP port on the server.

- Resource — The gateway resource that controls access to this gateway; in the default CPF, this is %Gateway_Object.

- Important:

- If no resource is specified, the g ateway is public, but InterSystems strongly recommends protecting all gateways using this mechanism.

- DotNetVersion — Specifies the .NET v ersion to be used. Possible values are N6.0, N5.0, F4.6.2, F4.5, F3.5, F2.0, and
C2.1; the default is N6.0.

Note:

The default of N6.0 is specified in the def ault CPF to ensure it is set when an instance is upgraded from a prior version in which the default was F4.5.

FilePath — Location of the gateway executable, used in assembling the command to start the gateway on the local server. If this setting is not specified, the def ault directory install-dir\dev\dotnet\bin\ is used, with the appropriate subdirectory selected according to the DotNetVersion setting, for example if DotNetVersion is N6.0 the FilePath would be install-dir\dev\dotnet\bin\net6.0.

Note:

Executables for DotNetVersion values F4.5, F2.0 and C2.1 are not installed with the current version of InterSystems IRIS, but the values are provided to support use of these executables from an earlier version.

Exec32 — On 64-bit platforms, a value of 1 indicates that the gateway is to be executed as 32-bit; the default is 0, for
64–bit.

The remaining eight fields can be used in an y gateway definition, and are sho wn in order in the following:

- SSLConfigur ationServer — Name of the SSL/TLS configuration to be used for serv er TLS/SSL.

- SSLConfigur ationClient — Name of the SSL/TLS configuration to be used for client TLS/SSL.

%DotNet Server

- VerifySSLHostName — Whether the TLS/SSL client should perform hostname verification. The default is 0 (no hostname verification).

- UseSharedMemory — Whether to use shared memory for connection if available. The default is 1 (use shared memory).

- LogFile — Full pathname of the file used to log all communication between InterSystems IRIS and the g ateway. This optional property should only be used when debugging. The maximum length is 1023 characters.

- AllowedIpAddresses — IP addresses that allow incoming connections. Specify 0.0.0.0 to allow connections on all IP addresses local to the machine (127.0.0.1, VPN address, and so on). You can also specify a single existing local IP address to restrict connections to that IP address. The default is 127.0.0.1.

- ConnectionTimeout — Number of seconds to wait for a connection with the gateway to be established. The range is
2-300; the default value is 5.

- InitializationTimeout — Number of seconds to wait for a response during initialization of the gateway. The range is
2-300; the default value is 5.

For information about changing and adding gateway definitions, see [Gateways].

[Gateways]

%IntegratedML Server

Define an Inte gratedML gateway.

Synopsis

[Gateways]
%IntegratedML Server=ML,Port,[optional fields]

Description
%IntegratedML Server is the name of the IntegratedML gateway definition in the def ault CPF, but additional IntegratedML gateways with different names can be created and existing gateways can be edited or deleted as described in [Gate-
ways]. Each definition consists of a name follo wed by = and then a comma-separated list of values defining the g ateway;
some fields are required, and others can be left blank, in which case def ault values are used. Fields specific to .Inte gratedML (and other Java-based) gateways and those that can be used with all gateways are described in the following two sections. For a more detailed description, see Java External Server Setup and Defining External Serv er Configurations for Ja va.

IntegratedML Gateway Fields The first three fields in an Inte
Java-based gateways. These are shown in order in the following:

gratedML gateway definition are common to all g ateways and the next three are specific to

- Type (required) — Type of the gateway. When defining an Inte gratedML gateway, this is set to ML.

- Port (required) — TCP port number for communication between the gateway and the proxy classes in InterSystems IRIS. This port number must not conflict with an y other local TCP port on the server.

- Resource — The gateway resource that controls access to this gateway; for %Integrated_ML Server in the default
CPF, this is %Gateway_ML.

- Important:

- If no resource is specified, the g ateway is public, but InterSystems strongly recommends protecting all gateways using this mechanism.

- ClassPath — The files that must be passed as an ar gument when starting the Java Virtual Machine (JVM), which are typically the files containing the classes used via the g ateway. The classpath must be properly quoted, with the correct platform-specific separators between files.

JVMArgs — Optional arguments passed to the JVM, used in the command to start the gateway.

JavaHome — Location of the JVM, used in the command to start the Gateway.

Important:

This field is required unless a def ault JVM is set for the host system.

The remaining eight fields can be used in an y gateway definition, and are sho wn in the General Gateway Fields section

For information about changing and adding gateway definitions, see [Gateways].

%JDBC Server

%JDBC Server

Define a JDBC g ateway.

Synopsis

[Gateways]
%JDBC Server=JDBC,Port,[optional fields]

Description
%JDBC Server is the name of the JDBC gateway definition in the def ault CPF, but additional JDBC gateways with different names can be created and existing gateways can be edited or deleted as described in [Gateways]. Each definition
consists of a name followed by = and then a comma-separated list of values defining the g ateway; some fields are required,
and others can be left blank, in which case default values are used. Fields specific to JDBC (and other Ja va-based) gateways and those that can be used with all gateways are described in the following two sections. For a more detailed description, see Java External Server Setup and Defining External Serv er Configurations for Ja va.

JDBC Gateway Fields The first three fields in a JDBC g
gateways. These are shown in order in the following:

ateway definition are common to all g ateways and the next three are specific to Ja va-based

- Type (required) — Type of the gateway. When defining a JDBC g ateway, this is set to JDBC.

- Port (required) — TCP port number for communication between the gateway and the proxy classes in InterSystems IRIS. This port number must not conflict with an y other local TCP port on the server.

- Resource — The gateway resource that controls access to this gateway; for %JDBC Server in the default CPF, this
is %Gateway_SQL.

- Important:

- If no resource is specified, the g ateway is public, but InterSystems strongly recommends protecting all gateways using this mechanism.

- ClassPath — The files that must be passed as an ar gument when starting the Java Virtual Machine (JVM), which are typically the files containing the classes used via the g ateway. The classpath must be properly quoted, with the correct platform-specific separators between files.

JVMArgs — Optional arguments passed to the JVM, used in the command to start the gateway.

JavaHome — Location of the JVM, used in the command to start the Gateway.

Important:

This field is required unless a def ault JVM is set for the host system.

The remaining eight fields can be used in an y gateway definition, and are sho wn in the General Gateway Fields section

For information about changing and adding gateway definitions, see [Gateways].

[Gateways]

%Java Server

Define a Ja va gateway.

Synopsis

[Gateways]
%Java Server=Java,Port,[optional fields]

Description
%Java Server is the name of the Java gateway definition in the def ault CPF, but additional Java gateways with different names can be created and existing gateways can be edited or deleted as described in [Gateways]. Each definition consists
of a name followed by = and then a comma-separated list of values defining the g ateway; some fields are required, and
others can be left blank, in which case default values are used. Fields specific to Ja va (and other Java-based) gateways and those that can be used with all gateways are described in the following two sections. For a more detailed description, see Java External Server Setup and Defining External Serv er Configurations for Ja va.

Java Gateway Fields The first three fields in a Ja
gateways. These are shown in order in the following:

va gateway definition are common to all g ateways and the next three are specific to Ja va-based

- Type (required) — Type of the gateway. When defining a Ja va gateway, this is set to Java.

- Port (required) — TCP port number for communication between the gateway and the proxy classes in InterSystems IRIS. This port number must not conflict with an y other local TCP port on the server.

- Resource — The gateway resource that controls access to this gateway; for %Java Server in the default CPF, this
is %Gateway_Object.

- Important:

- If no resource is specified, the g ateway is public, but InterSystems strongly recommends protecting all gateways using this mechanism.

- ClassPath — The files that must be passed as an ar gument when starting the Java Virtual Machine (JVM), which are typically the files containing the classes used via the g ateway. The classpath must be properly quoted, with the correct platform-specific separators between files.

JVMArgs — Optional arguments passed to the JVM, used in the command to start the gateway.

JavaHome — Location of the JVM, used in the command to start the Gateway.

Important:

This field is required unless a def ault JVM is set for the host system.

The remaining eight fields can be used in an y gateway definition, and are sho wn in the General Gateway Fields section

For information about changing and adding gateway definitions, see [Gateways].

%Python Server

%Python Server

Define a Python g ateway.

Synopsis

[Gateways]
%Python Server=Python,Port,[optional fields]

Description
%Python Server is the name of the Python gateway definition in the def ault CPF, but additional Python gateways with different names can be created and existing gateways can be edited or deleted as described in [Gateways]. Each definition
consists of a name followed by = and then a comma-separated list of values defining the g ateway; some fields are required,
and others can be left blank, in which case default values are used. Fields specific to Python g ateways and those that can be used with all gateways are described in the following two sections. For a more detailed description, see Python External Server Setup and Defining External Serv er Configurations for Python.

Python Gateway Fields The first three fields in a Python g
gateways. These are shown in order in the following:

ateway definition are common to all g ateways and the next three are specific to Python

- Type (required) — Type of the gateway. When defining a Python g ateway, this is set to Python.

- Port (required) — TCP port number for communication between the gateway and the proxy classes in InterSystems IRIS. This port number must not conflict with an y other local TCP port on the server.

- Resource — The gateway resource that controls access to this gateway; for %Python Server in the default CPF,
this is %Gateway_SQL.

- Important:

- If no resource is specified, the g ateway is public, but InterSystems strongly recommends protecting all gateways using this mechanism.

- PythonPath — Location of the Python interpreter, used in assembling the command to start the gateway on the local server.

Important:

This field is required unless a def ault Python interpreter is set for the host system.

PythonOptions — Python options to be included in the command to start the gateway on the local server.

(not used) — Leave this field empty (consecuti ve commas).

The remaining eight fields can be used in an y gateway definition, and are sho wn in the General Gateway Fields section

For information about changing and adding gateway definitions, see [Gateways].

[Gateways]

%R Server

Define an R g ateway.

Synopsis

[Gateways]
%R Server=R,Port,[optional fields]

Description
%R Server is the name of the R gateway definition in the def ault CPF, but additional R gateways with different names can be created and existing gateways can be edited or deleted as described in [Gateways]. Each definition consists of a
name followed by = and then a comma-separated list of values defining the g ateway; some fields are required, and others
can be left blank, in which case default values are used. Fields specific to R (and other Ja va-based) gateways and those that can be used with all gateways are described in the following two sections. For a more detailed description, see Java External Server Setup and Defining External Serv er Configurations for Ja va.

R Gateway Fields The first three fields in an R g
gateways. These are shown in order in the following:

ateway definition are common to all g ateways and the next three are specific to Ja va-based

- Type (required) — Type of the gateway. When defining an R g ateway, this is set to R.

- Port (required) — TCP port number for communication between the gateway and the proxy classes in InterSystems IRIS. This port number must not conflict with an y other local TCP port on the server.

- Resource — The gateway resource that controls access to this gateway; for %R Server in the default CPF, this is
%Gateway_Object.

- Important:

- If no resource is specified, the g ateway is public, but InterSystems strongly recommends protecting all gateways using this mechanism.

- ClassPath — The files that must be passed as an ar gument when starting the Java Virtual Machine (JVM), which are typically the files containing the classes used via the g ateway. The classpath must be properly quoted, with the correct platform-specific separators between files.

JVMArgs — Optional arguments passed to the JVM, used in the command to start the gateway.

JavaHome — Location of the JVM, used in the command to start the Gateway.

Important:

This field is required unless a def ault JVM is set for the host system.

The remaining eight fields can be used in an y gateway definition, and are sho wn in the General Gateway Fields section

For information about changing and adding gateway definitions, see [Gateways].

%XSLT Server

%XSLT Server

Define an XSL T gateway.

Synopsis

[Gateways]
%XSLT Server=XSLT,Port,[optional fields]

Description
%XSLT Server is the name of the XSLT gateway definition in the def ault CPF, but additional XSLT gateways with different names can be created and existing gateways can be edited or deleted as as described in [Gateways]. Each definition
consists of a name followed by = and then a comma-separated list of values defining the g ateway; some fields are required,
and others can be left blank, in which case default values are used. Fields specific to XSL T (and other Java-based) gateways and those that can be used with all gateways are described in the following two sections. For a more detailed description, see Java External Server Setup and Defining External Serv er Configurations for Ja va.

XSLT Gateway Fields The first three fields in an XSL T gateway definition are common to all g ateways and the next three are specific to Ja va-
based gateways. These are shown in order in the following:

- Type (required) — Type of the gateway. When defining an XSL T gateway, this is set to XSLT.

- Port (required) — TCP port number for communication between the gateway and the proxy classes in InterSystems IRIS. This port number must not conflict with an y other local TCP port on the server.

- Resource — The gateway resource that controls access to this gateway; for %XSLT Server in the default CPF, this
is %Gateway_Object.

- Important:

- If no resource is specified, the g ateway is public, but InterSystems strongly recommends protecting all gateways using this mechanism.

- ClassPath — The files that must be passed as an ar gument when starting the Java Virtual Machine (JVM), which are typically the files containing the classes used via the g ateway. The classpath must be properly quoted, with the correct platform-specific separators between files.

JVMArgs — Optional arguments passed to the JVM, used in the command to start the gateway.

JavaHome — Location of the JVM, used in the command to start the Gateway.

Important:

This field is required unless a def ault JVM is set for the host system.

The remaining eight fields can be used in an y gateway definition, and are sho wn in the General Gateway Fields section

For information about changing and adding gateway definitions, see [Gateways].

[Gateways]

Remote Server

Define a remote g ateway.

Synopsis

[Gateways]
Name=Remote,Port,Resource,Address,(not used),(not used),[optional fields]

Description
There is no remote server defined in the [Gateways] section of the default CPF, but remote gateways can be created and existing gateways can be edited or deleted as described in [Gateways]. Each definition consists of a name follo wed by =
and then a comma-separated list of values defining the g ateway; some fields are required, and others can be left blank, in
which case default values are used. Fields specific to remote g ateways and those that can be used with all gateways are described in the following two sections.

Remote Gateway Fields The first three fields in a remote g
gateways. These are shown in order in the following:

ateway definition are common to all g ateways and the next three are specific to remote

- Type (required) — Type of the gateway. When defining a remote g ateway, this is set to remote.

- Port (required) — TCP port number for communication between the gateway and the proxy classes in InterSystems IRIS. This port number must not conflict with an y other local TCP port on the remote server.

- Resource — The gateway resource that controls access to this gateway. For a remote gateway created using the Management Portal (see [Gateways]), the default is %Gateway_Object.

- Important:

- If no resource is specified, the g ateway is public, but InterSystems strongly recommends protecting all gateways using this mechanism.

- Address — The IP address or hostname of the remote system on which you are defining the g ateway.

(not used) — Leave this field empty (consecuti ve commas).

(not used) — Leave this field empty (consecuti ve commas).

The remaining eight fields can be used in an y gateway definition, and are sho wn in the General Gateway Fields section

For information about changing and adding gateway definitions, see [Gateways].

[IO]

The parameters found in the [IO] section of the CPF allow you to change the default mnemonic (^%X364) for WRITE commands. You can also find these settings on the IO Settings page of the Management Portal (System Administration > Configuration > Device Settings > IO Settings).

[IO]

Other

Set the default mnemonic for WRITE commands to device types other than terminal or sequential file.

Synopsis

[IO] Other=n

n is an InterSystems IRIS® data platform routine name. The default is ^%X364.

Description
The Other setting specifies the def ault mnemonic for device types other than terminal or sequential file. When an OPEN or USE command includes no mnemonic space argument, InterSystems IRIS uses the default mnemonic for that device type. For more information, see Controlling Devices with Mnemonic Spaces.

On the IO Settings page of the Management Portal (System Administration > Configuration > Device Settings > IO Settings), in the Other row, enter an InterSystems IRIS routine name.

Instead of using the Management Portal, you can change Other in the Config.IO class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

File

File

Set the default mnemonic for WRITE commands to a sequential file.

Synopsis

[IO] File=n

n is an InterSystems IRIS® data platform routine name. The default is ^%X364.

Description
The File setting specifies the def ault mnemonic for sequential files. When an OPEN or USE command includes no mnemonic space argument, InterSystems IRIS uses the default mnemonic for that device type. For more information, see Controlling Devices with Mnemonic Spaces.

On the IO Settings page of the Management Portal (System Administration > Configuration > Device Settings > IO Settings), in the File row, enter an InterSystems IRIS routine name.

Instead of using the Management Portal, you can change File in the Config.IO class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[IO]

Terminal

Set the default mnemonic for WRITE commands to a terminal device.

Synopsis

[IO] Terminal=n

n is an InterSystems IRIS® data platform routine name. The default is ^%X364.

Description
The Terminal setting specifies the def ault mnemonic for terminal devices. When an OPEN or USE command includes no mnemonic space argument, InterSystems IRIS uses the default mnemonic for that device type. For more information, see Controlling Devices with Mnemonic Spaces.

On the IO Settings page of the Management Portal (System Administration > Configuration > Device Settings > IO Settings), in the Terminal row, enter an InterSystems IRIS routine name.

Instead of using the Management Portal, you can change Terminal in the Config.IO class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[Journal]

This topic describes the parameters found in the [Journal] section of the configuration parameter file.

[Journal]

AlternateDirectory

Set the alternate location of the journal file.

Synopsis

[Journal] AlternateDirectory=n

n is the full path to an existing directory. By default, no directory is listed.

Description
AlternateDirectory is the name of an alternate (secondary) directory in which to store journal files.

This alternate directory is used if the primary journal directory specified by CurrentDirectory is unavailable; for
example, if the disk partition for the primary journal directory is full, offline, or has some other problem. F or these reasons, InterSystems recommends that the alternate journal directory be located on a different disk than the current journal directory.

When installed, the initial value of this field is an empty string. After InterSystems IRIS® data platform starts for the first time, the actual journal directory is filled in here, such as <install-dir>\mgr\journal.

The combined length of AlternateDirectory and the JournalFilePrefix parameters must be less than 208 characters. (The maximum length of JournalFilePrefix is 64 characters.)

On the Journal Settings page of the Management Portal (System Administration > Configuration > System Configuration > Journal Settings) , in the Secondary journal directory row, select Browse. Select the name of an existing directory.

Instead of using the Management Portal, you can change AlternateDirectory in the Config.Journal class (as described in the class reference), from the ^JOURNAL utility (as described in Update Journal Settings Using ^JRNOPTS), or by editing the CPF in a text editor (as described in Editing the Active CPF).

ArchiveName

ArchiveName

Specify where to copy journal files when archi ving them.

Synopsis

[Journal] ArchiveName=archivename

Where archivename is the name of an archive target. There is no default.

Description
ArchiveName is the name of an archive target defined in the [Archives] section. .

On the Journal Settings page of the Management Portal (System Administration > Configuration > System Configuration > Journal Settings), in the Archive journal files section, select an existing target from the To Archive Target dropdown list.

Instead of using the Management Portal, you can change ArchiveName in the Config.Journal class (as described in the class reference), from the ^JOURNAL utility (as described in Update Journal Settings Using ^JRNOPTS), or by editing the CPF in a text editor (as described in Editing the Active CPF).

[Journal]

BackupsBeforePurge

Set the number of backups before InterSystems IRIS® data platform purges finished journal files.

Synopsis

[Journal] BackupsBeforePurge=n

n is an integer in the range 0—10. The default value is 2.

Description
BackupsBeforePurge defines when InterSystems IRIS pur ges a finished journal file (that is, a journal file that is no longer in progress). The value n is a number of successful InterSystems IRIS instance backups that must take place before the corresponding journal files can be pur ged.

BackupsBeforePurge relates to DaysBeforePurge. If both are greater than 0, files are pur ged after n days or n successful backups, whichever indicates the shorter time period. If BackupsBeforePurge is 0, purging is done solely based
on DaysBeforePurge; if DaysBeforePurge is 0, then purging is done solely based on BackupsBeforePurge. If
both are 0, the automatic purging of journal files (and journal history) is disabled and journal files are not pur

ged.

No journal file containing currently open transactions is pur ged, even if it meets the above criteria.

Note:

If PurgeArchived is 1, this setting is ignored.

For details about journal files, see Overview of Journaling.

On the Journal Settings page of the Management Portal (System Administration > Configuration > System Configuration >
Journal Settings), in the When to purge journal files category, choose one of the following:

- To purge journal files based on a number of days, choose After this many days and enter a number of days.

- To purge journal files based on a number of backups, choose After this many successive successful backups and enter a number of backups.

Instead of using the Management Portal, you can change BackupsBeforePurge in the Config.Journal class (as described in the class reference), from the ^JOURNAL utility (as described in Update Journal Settings Using ^JRNOPTS), or by editing the CPF in a text editor (as described in Editing the Active CPF).

CompressFiles

CompressFiles

Specify whether to compress completed journal files.

Synopsis

[Journal] CompressFiles=n

n is either 1 or 0. The default is 1.

Description
When CompressFiles is enabled (n=1), InterSystems IRIS® data platform automatically compresses finished journal files. The system periodically scans for completed journal files and compresses them using Zstd compression. The active journal file is not compressed until it is completed.

Compressed journal files retain the same name, b ut with a z added to the end. For example, the journal file 20210818.001, when compressed, becomes 20210818.001z. Most InterSystems IRIS functions and utilities are able to access a compressed journal file by its original name, ignoring the z.

For details about journal files, see Overview of Journaling.

On the Journal Settings page of the Management Portal (System Administration > Configuration > System Configuration > Journal Settings), select Compress journal files to enable this setting.

Instead of using the Management Portal, you can change CompressFiles in the Config.Journal class (as described in the class reference), from the ^JOURNAL utility (as described in Update Journal Settings Using ^JRNOPTS), or by editing the CPF in a text editor (as described in Editing the Active CPF).

[Journal]

CurrentDirectory

Set the primary location of the journal file.

Synopsis

[Journal] CurrentDirectory=n

n is the full path to an existing directory. The default is <install-dir>\mgr\journal\.

Description
CurrentDirectory is the name of a directory in which to store the journal files (the primary directory). When installed, the initial value of this field is an empty string. After InterSystems IRIS® data platform starts for the first time, the actual journal directory is filled in here, such as <install-dir>\mgr\journal.

The combined length of CurrentDirectory and the JournalFilePrefix parameters must be less than 208 characters. (The maximum length of JournalFilePrefix is 64 characters.)

On the Journal Settings page of the Management Portal (System Administration > Configuration > System Configuration > Journal Settings), in the Primary journal directory row, select Browse. Select the name of an existing directory.

Instead of using the Management Portal, you can change CurrentDirectory in the Config.Journal class (as described in the class reference), from the ^JOURNAL utility (as described in Update Journal Settings Using ^JRNOPTS), or by editing the CPF in a text editor (as described in Editing the Active CPF).

DaysBeforePurge

DaysBeforePurge

Set the number of days before InterSystems IRIS® data platform purges finished journal files.

Synopsis

[Journal] DaysBeforePurge=n

n is an integer in the range 0—100. The default value is 2.

Description
DaysBeforePurge defines when InterSystems IRIS pur ges a finished journal file (that is, a journal file that is no longer in progress). The value n is a number days that elapse before the corresponding journal files can be pur ged.

BackupsBeforePurge relates to DaysBeforePurge. If both are greater than 0, files are pur ged after n days or n successful backups, whichever indicates the shorter time period. If BackupsBeforePurge is 0, purging is done solely based
on DaysBeforePurge; if DaysBeforePurge is 0, then purging is done solely based on BackupsBeforePurge. If
both are 0, the automatic purging of journal files (and journal history) is disabled and journal files are not pur

ged.

No journal file containing currently open transactions is pur ged, even if it meets the above criteria.

Note:

If PurgeArchived is 1, this setting is ignored.

For details about journal files, see Overview of Journaling.

On the Journal Settings page of the Management Portal (System Administration > Configuration > System Configuration >
Journal Settings), in the When to purge journal files category, choose one of the following:

- To purge journal files based on a number of days, choose After this many days and enter a number of days.

- To purge journal files based on a number of backups, choose After this many successive successful backups and enter a number of backups.

Instead of using the Management Portal, you can change DaysBeforePurge in the Config.Journal class (as described in the class reference), from the ^JOURNAL utility (as described in Update Journal Settings Using ^JRNOPTS), or by editing the CPF in a text editor (as described in Editing the Active CPF).

[Journal]

FileSizeLimit

Set the maximum size of a journal file.

Synopsis

[Journal] FileSizeLimit=n

n is an integer in the range 0—4079 (MB). The default value is 1024.

Description
FileSizeLimit is the maximum size of the journal file, in me gabytes. When a journal file gro ws to this size it is closed and a new journal file is created. F or more information about journal file rollo ver, see Journal File Names and Rollover.

The FileSizeLimit and jrnbufs parameters have a maximum combined size limit of 4 GB; see Configuring Journal
Settings for more information.

On the Journal Settings page of the Management Portal (System Administration > Configuration > System Configuration > Journal Settings) , in the Start new journal file every (MB) row, enter a number of megabytes.

Instead of using the Management Portal, you can change FileSizeLimit in the Config.Journal class (as described in the class reference), from the ^JOURNAL utility (as described in Update Journal Settings Using ^JRNOPTS), or by editing the CPF in a text editor (as described in Editing the Active CPF).

FreezeOnError

FreezeOnError

Allow journaling freeze when a journal I/O error occurs.

Synopsis

[Journal] FreezeOnError=n

n is either 1 or 0. The default value is 0.

Description
When FreezeOnError=0 (false, the default), InterSystems IRIS does not freeze journaling on a journal file I/O error . This option keeps the instance available, but exposes it to potential data loss.

The journal daemon retries the failed operation periodically (typically at one second intervals) until either it succeeds, or journaling is disabled because the instance cannot buffer any further journaled updates or a predetermined time limit (typically 150 seconds) has been reached.

Important: When journaling is disabled, you should back up your databases as soon as possible. Continuing without

journaling is a calculated risk, as it means the activity that occurs during this period cannot be restored.

Once journaling has been disabled, you must manually restart it, which you can do by running the ^JRNSTART routine or selecting option 1, Begin Journaling, from the ^JOURNAL routine menu.

When FreezeOnError=1 (true), InterSystems IRIS immediately freezes all journaled global updates when a journal file I/O error occurs. Global updates are also frozen if the journal daemon has been unable to complete a journal write for at least 30 seconds. This option protects the instance against data loss, but makes it less available or unavailable while the problem is being resolved.

The journal daemon retries the failed I/O operation and unfreezes global updates after it succeeds. Meanwhile, the freezing of global updates causes other jobs to hang. The typical outcome is that InterSystems IRIS hangs until you resolve the journaling problem, with the system appearing to be down to operational end-users. While InterSystems IRIS is hung you can take corrective measures, such as freeing up disk space, switching the journal to a different disk, or correcting a hardware failure.

Important:

The FreezeOnError setting is automatically overridden and turned on when an instance is a failover member of a mirror.

For further details, see Journal I/O Errors.

On the Journal Settings page of the Management Portal (System Administration > Configuration > System Configuration > Journal Settings), select FreezeOnError to enable this setting.

Instead of using the Management Portal, you can change FreezeOnError in the Config.Journal class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[Journal]

JournalFilePrefix

Set a journal file name prefix.

Synopsis

[Journal] JournalFilePrefix=n

n is an alphanumeric string. The maximum length is 64 characters. The default is an empty string.

Description
JournalFilePrefix is a string added to the start of the journal file name. F or example, if JournalFilePrefix is set to Oct, the resulting journal file w ould look like Oct20221001.001.

On the Journal Settings page of the Management Portal (System Administration > Configuration > System Configuration > Journal Settings), at the Journal file prefix row, enter a prefix.

Instead of using the Management Portal, you can change JournalFilePrefix in the Config.Journal class (as described in the class reference), from the ^JOURNAL utility (as described in Update Journal Settings Using ^JRNOPTS), or by editing the CPF in a text editor (as described in Editing the Active CPF).

JournalcspSession

JournalcspSession

Allow journaling of web sessions.

Synopsis

[Journal] JournalcspSession=n

n is either 1 or 0. The default value is 0.

Description
When JournalcspSession is enabled (n = 1), InterSystems IRIS® data platform journals the ^%cspSession global. Enable this setting if you want the web session global to be replicated onto another machine for failover or if you want a web session to survive an InterSystems IRIS restart. Otherwise, the ^%cspSession global is mapped to IRISTEMP and not journaled. InterSystems IRIS kills the ^%cspSession global on system restart or upgrade to a new InterSystems IRIS software version, so that any record of ongoing web sessions is removed.

On the Journal Settings page of the Management Portal (System Administration > Configuration > System Configuration > Journal Settings), select Journal Web session to enable this setting.

Instead of using the Management Portal, you can change JournalcspSession in the Config.Journal class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[Journal]

PurgeArchive

Specify whether to purge journal files as soon as the y are copied to the archive target.

Synopsis

[Journal] PurgeArchive=boolean

Where boolean is 1 or 0. The default is 0.

Description
PurgeArchive specifies whether to pur ge journal files as soon as the y are copied to the archive target. Necessary files are never purged, no matter what this setting is.

If PurgeArchive is 1, the settings BackupsBeforePurge and DaysBeforePurge are both ignored.

On the Journal Settings page of the Management Portal (System Administration > Configuration > System Configuration > Journal Settings), in the Archive journal files section, select the As soon as they are copied to archive check box. (It is necessary to choose an archive target, in order to see this option.)

Instead of using the Management Portal, you can change PurgeArchive in the Config.Journal class (as described in the class reference), from the ^JOURNAL utility (as described in Update Journal Settings Using ^JRNOPTS), or by editing the CPF in a text editor (as described in Editing the Active CPF).

[LicenseServers]

This topic describes LicenseServer, the parameter found in the [LicenseServer] section of the CPF. There can be multiple license servers defined.

[LicenseServers]

LicenseServers

Define license serv ers.

Synopsis

[LicenseServers] Name=IpAddress, Port[, KeyDirectory]

Description
The [LicenseServers] section contains an entry for every license server configured for InterSystems IRIS® data
platform. The entries are as follows:

- IpAddress— IP address of the license server.

- Port— UDP port number used by the license server. The port numbers used at different IP addresses do not need to be different. However, the license server port number used at each IP address must be different from any UDP port number used at that IP address.

- KeyDirectory— License key directory. This setting is optional. If a directory is specified, on startup the License Serv er will read all valid .key files from that directory .

For more information about license servers, see Configuring InterSystems IRIS Licensing .

On the License Servers page of the Management Portal (System Administration > Licensing > License Servers), select the name of a license server to edit it, or click Create License Server to add a new license server.

[Logging]

This topic describes the parameters found in the [Logging] section of the CPF.

[Logging]

ChildProcessLaunchCommand

Set the pipe command specifying where the system sends the structured log data.

Synopsis

[Logging] ChildProcessLaunchCommand=Executable Options

The default is of the form:

ChildProcessLaunchCommand=irislogd.exe -f /path/to/myfilename.log

Description
The ChildProcessLaunchCommand specifies where the system sends structured log data. It tak es a target executable that receives the log data. You can optionally specify arguments for the executable.

For the default irislogd executable, you can pass the following arguments:

Argument

-d

-e errfilename

-f logfilename

-h hostname

Purpose

Emit diagnostic and error messages

Write errors and diagnostic messages to the given file.

Write log messages to the given file.

Includes the given host name in the structured log file.

-i irisinstance

Includes the given instance name in the structured log file.

-s

Write log messages to the Unix® syslog facility (Unix® only)

You can change this parameter using the Management Portal, the ^LOGDMN routine, or the class-based API (all described in Configure Structured Logging ). Alternatively, you can edit the CPF in a text editor (as described in Editing the Active
CPF).

Enabled

Enabled

Allow structured logging data collection.

Synopsis

[Logging] Enabled=n

n is either 1 (true) or 0 (false). The default value is 0.

Description
When you enable structured logging (Enabled=1), the log daemon automatically activates when InterSystems IRIS® data platform starts up. However, setting this to 1 will NOT start the log daemon. If this parameter is set to 0, any attempt to start the log daemon fails.

You can change this parameter using the Management Portal, the ^LOGDMN routine, or the class-based API (all described in Configure Structured Logging ). Alternatively, you can edit the CPF in a text editor (as described in Editing the Active
CPF).

[Logging]

EventFilter

Optional filter to restrict e vents in the structured log based on event type.

Synopsis

[Logging] EventFilter=n

n is the list of patterns to be included and excluded from the structured log.

Description
Filter patterns can be specified in the follo wing format:

+<pattern>: Include events matching <pattern> -<pattern>: Exclude events matching <pattern> <pattern>: Default include (equivalent to +<pattern>)

Consider the following syntactical rules when specifying filter patterns:

- Use a period (.) as a separator for hierarchical names, such as A.B.C

- Use an asterisk (*) as a wildcard only at the end of patterns, such as A.B.*, which includes all sub-events under A.B

- Use a comma (,) to separate multiple filter patterns, such as -A,A.B

For example:

-Audit.*,Audit.LoginFailure

Excludes all Audit events except LoginFailure.

Events not specified in the filter are included. Specific rules tak have higher priority. For example, -A.B.C has a higher priority than +A.

e precedence over wildcard rules, and more detailed patterns

You can change this parameter using the Management Portal, the ^LOGDMN routine, or the class-based API (all described in Configure Structured Logging ). Alternatively, you can edit the CPF in a text editor (as described in Editing the Active
CPF).

Format

Format

The format of the data sent to the pipe.

Synopsis

[Logging] Format=n

n is either name/value pair (NVP) or JSON. The default value is NVP.

Description
The log daemon can format output for structured logging in NVP or JSON format. In the NVP format, each line in the file contains a set of name/value pairs separated by spaces. Each name/value pair has the form name=value, and if value includes a space character, then value is enclosed in parentheses. In the JSON format, each line in the file is a JSON object with a set of properties.

You can change this parameter using the Management Portal, the ^LOGDMN routine, or the class-based API (all described in Configure Structured Logging ). Alternatively, you can edit the CPF in a text editor (as described in Editing the Active
CPF).

[Logging]

Interval

Set the interval in seconds between successive calls to the pipe command.

Synopsis

[Logging] Interval=n

n is a number in the range of 0.01–3600. The default value is 10.

Description
Interval represents the number of seconds the log daemon waits before scanning the messages.log file and %SYS.Audit log for new entries. It may be expressed as a decimal up to two digit points and can be no less than 0.01 (10 ms) and no larger than 3600 (equivalent to 1 hour).

You can change this parameter using the Management Portal, the ^LOGDMN routine, or the class-based API (all described in Configure Structured Logging ). Alternatively, you can edit the CPF in a text editor (as described in Editing the Active
CPF).

Level

Level

Set the minimum log level.

Synopsis

[Logging] Level=n

n is one of the following:

- DEBUG2

- DEBUG

- INFO

- WARN

- SEVERE

- FATAL The default value is WARN.

Description
This parameter represents the minimum log level of detail, according to the following:

- DEBUG2 — detailed debug messages (such as hex dumps).

- DEBUG — less detailed debug messages.

- INFO — informational messages, including all audit events.

- WARN (the default) — warnings, which indicate problems that may need attention but that have not disrupted operations.

- SEVERE — severe errors, which indicate problems that have disrupted operations.

- FATAL — fatal errors, which indicate problems have caused the system not to run.

You can change this parameter using the Management Portal, the ^LOGDMN routine, or the class-based API (all described in Configure Structured Logging ). Alternatively, you can edit the CPF in a text editor (as described in Editing the Active
CPF).

[Map]

This topic describes the parameters found in the [Map] section of the CPF. It also describes parameters that may be present in sections called [Map.w], where w is the name of an InterSystems IRIS® data platform namespace.

[Map]

Global

Map globals to namespaces.

Synopsis

[Map.w] Global_GlobalName(ss)=Database,Collation,LockLocation

Description
Whenever a namespace (w) contains mappings, InterSystems IRIS® data platform creates a section in the configuration parameter (CPF) file called [Map.w] for that namespace. For example, mappings for the USER namespace appear under the section [Map.USER]. The [Map.w] section contains every global mapping, routine mapping, and package mapping for that namespace.

Global mappings take the form Global_GlobalName, where GlobalName is the specific global that is being mapped. Each global entry contains three comma-separated values that map a global to a namespace. Only the first v alue is required.
If the other values are not specified, the y are set to the instance default. These values are:

- Database — Database location of the global.

- Collation — Collation of the global (Default=5, InterSystems IRIS standard collation).

- LockLocation — Lock database location for the global (Default=Database location). InterSystems recommends that the lock database be the same as the Database location.

The collation setting is ignored if the global is not mapped at the subscript level. If the actual collation of the global does vel mapped globals), a <COLLATEMISMATCH> not match the defined collation in the namespace definition (for subscript le error is generated when it is referenced. This is because InterSystems IRIS requires the global to have the same collation across all the databases it lives in, regardless of the default collation for the particular database.

For more information about mappings, see Add Global, Routine, and Package Mapping to a Namespace.

Subscript Mapping

You may specify subscript mapping as part of the GlobalName (ss). Note that if a subscript mapping is specified, a higher level mapping of the global itself must also exist. So if you want to create a mapping ^X(9), then a mapping for ^X must
also exist. Subscript mappings may take the following forms:

(1)

(“A”)

(1):(5) — from 1 up to, but not including, 5

("A"):("Z") — from A up to, but not including, Z

(BEGIN):("X") — from the beginning up to, but not including, X

("Y"):(END) — from Y up to the end

Examples

Map the global ^SALES to the SALES Database:

Global_SALES=SALES

Subscript map the global ^SALES(“MA”) to the SALESMA database:

Global_SALES(“MA”)=SALESMA

Map the global ^ACCOUNT to the database ACCOUNTS:

Global_ACCOUNT=ACCOUNTS

Subscript map the global ^ACCOUNT(1) up to but not including ACCOUNT(5) to database ACCOUNTS1TO4:

Global

Global_ACCOUNT(1):(5)=ACCOUNTS1TO4

Map all globals starting with ABC to database ABC:

Global_ABC*=ABC

On the Namespaces page of the Management Portal (System Administration > Configuration > System Configuration > Namespaces), select Global Mappings for the namespace you want to explore. To add a new entry, click New. To edit an existing entry, select Edit in that entry's row.

[Map]

Package

Map packages to namespaces.

Synopsis

[Map.w] Package_PackageName=Database

Description
Whenever a namespace (w) contains mappings, InterSystems IRIS® data platform creates a section in the configuration parameter (CPF) file called [Map.w] for that namespace. For example, mappings for the USER namespace appear under the section [Map.USER]. The [Map.w] section contains every global mapping, routine mapping, and package mapping for that namespace.

Package mappings take the form Package_PackageName, where PackageName is the specific package that is being mapped. Each package entry contains a database location (Database) that contains the named package. Classes in the specified package become a vailable in the w namespace.

For more information about mappings, see Add Global, Routine, and Package Mapping to a Namespace.

Examples

Map package TEST to the USER database.

Package_TEST=USER

Map package TOOLS to the DEVELOPER database.

Package_TOOLS=DEVELOPER

On the Namespaces page of the Management Portal (System Administration > Configuration > System Configuration > Namespaces), select Package Mappings for the namespace you want to explore. To add a new entry, click New. To edit an existing entry, select Edit in that entry's row.

Routine

Routine

Map routines to namespaces.

Synopsis

[Map.w] Routine_RoutineName_Type=Database

Description
Whenever a namespace (w) contains mappings, InterSystems IRIS® data platform creates a section in the configuration parameter (CPF) file called [Map.w] for that namespace. For example, mappings for the USER namespace appear under the section [Map.USER]. The [Map.w] section contains every global mapping, routine mapping, and package mapping for that namespace.

Routine mappings take the form Routine_RoutineName, where RoutineName is the specific routine that is being mapped. Each routine entry contains a database location (Database) that contains the named routine. The specified routine become available in the w namespace.

Type is normally not specified. Type only needs to be specified if you w ant to map part of a routine to another database. Valid values for Type are: MAC, INT, INC, or OBJ.

For more information about mappings, see Add Global, Routine, and Package Mapping to a Namespace.

Examples

Map routine SALE to the SALES database:

Routine_SALE=SALES

Map all routines starting with ACC to the ACCOUNTS database:

Routine_ACC*=ACCOUNTS

Map the object code for routine TEST to the TEST database:

Routine_TEST_OBJ=TEST

On the Namespaces page of the Management Portal (System Administration > Configuration > System Configuration > Namespaces), select Routine Mappings for the namespace you want to explore. To add a new entry, click New. To edit an existing entry, select Edit in that entry's row.

[MapMirrors]

This topic describes the [MapMirrors] section of the CPF.

[MapMirrors]

MapMirrors

Define mirror members for mirrors that include this instance.

Synopsis

[MapMirrors.m] Name=a,b,c,d,e,f,g,h,i,j,k,l,m,n

Description
For each mirror (m) connected to the current instance, InterSystems IRIS® data platform creates a section in the CPF called [MapMirrors.m], which contains an entry for all instances connected to that mirror. The entries are made up of a Name
and 14 comma-separated values (a–n), as follows:

- Name – Required. Unique name identifying this member within the mirror. An uppercase alphanumeric string with a
maximum length of 15 characters, cannot contain spaces or tabs, commas (,), semicolons (;), or equal signs (=), and
is converted to uppercase before saving.

- This is the name of this instance in this mirror. Mirror system names must be unique across all of the mirrors as an instance has a single mirror system name which may appear in multiple mirror sets (that is, an async member may connect to multiple mirrors). For the most part the GUID is used to identify a mirror member, the Name is used for display purposes. The name cannot contain a colon (:) .

- a (AgentAddress) – The network address (IP address preferred to avoid DNS issues) that mirror members which connect to the primary should use to contact the Agent on this failover member. This is omitted on async members as the agent is not used for mirroring on those instances. This is required on failover members. The agent can transfer journal data so a private address may be desirable here to avoid network congestion.

- b (AgentPort) – Port # which the agent on this instance is configured to listen on.
##class(SYS.Agent).GetApplicationPort() returns the current value if the local agent is active.

- c – For internal use.

- d (SuperServerAddress) – The network address used to connect to the primary by external mirror-aware systems (currently only ECP application servers, although in the future this may extend to other connections). Other mirror members may connect to a member's superserver address for control and monitoring purposes. When a member is primary, an async member attempts to establish its data channel (over which it receives journal data) using this address if the primary’s mirror private address (MirrorPrivate) is not accessible.

- e (GUID) – Required. An internal GUID, unique to this mirror. Uniquly identifies this node in the mirror . Apart from identifying the nodes, primarily used to identify the instance that owns a particular copy of a mirrored database.

- f (InstanceDirectory) – The installation directory of the instance (the parent of the mgr directory). Used primarily on failover members to identify the instance to the agent.

- g (MemberType) – Numeric value indicating the type of mirror member. One of:

- –

- –

## 0 - Failover member

## 2 - Async member

h (MirrorPrivate) – When this instance is primary, other mirror members use this address to establish the mirror data channel, over which they receive journal data from the primary. Async members fall back to the primary’s superserver address (SuperServerAddress) if they cannot reach it at the mirror private address.

i (MirrorSSPort) – Superserver port for this instance. Used in conjunction with both the MirrorAddress and the ECPAddress by clients establishing connections to this instance.

j, k, l, m, and n – For internal use.

MapMirrors

Example

Each entry is on one line:

[MapMirrors.MIMI]
MIMI_A=mirrorhostA,2188,,mirrorhostA,C7BA9224-3851-47D4-83BD,c:\intersystems\20142302july10a\,0,mirrorhostA,56776,,0, MIMI_B=mirrorhostB,2188,,mirrorhostB,D14611B3-E0F5-4708-A111,c:\intersystems\20142302july10b\,0,mirrorhostB,56777,,0, MIMI_D=mirrorhostD,2188,,mirrorhostD,06E1D307-59D9-4500-AA3B,c:\intersystems\20142302jul10d\,2,mirrorhostD,56779,,0,

On the Create a Mirror page of the Management Portal (System Administration > Configuration > Mirror Settings > Create a Mirror), enter the requested information. You can edit an existing mirror on the Edit Mirror page, but only from the primary failover member.

[MirrorMember]

This topic describes the parameters found in the [MirrorMember] section of the CPF.

[MirrorMember]

AgentAddress

Not in use.

Description
Not in use.

AsyncMemberGUID

AsyncMemberGUID

Review async member GUID

Synopsis

[MirrorMember] AsyncMemberGUID=Name

Name is any alphanumeric string.

Description
You can create a mirror member called an async member, which can be configured to recei ve updates from one or more mirrors across the enterprise. This allows a single node to act as a comprehensive enterprise-wide data warehouse. Async members are not failover mirror members and, therefore, are not candidates for failover.

For more information, see Async Mirror Members.

Example

AsyncMemberGUID=06E1D307-59D9-4500-AA3B-4FF405E2A44D

You can change AsyncMemberGUID in the Config.MirrorMember class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[MirrorMember]

AsyncMemberType

Specify async type (Disaster recovery, read-only, or read-write).

Synopsis

[MirrorMember] AsyncMemberType=n

n is either 0, 1, or 2.

Description
AsyncMemberType indicates whether the async member is a disaster recovery (DR), read-only reporting, or read-write reporting async member.

- 0 - Disaster Recovery (DR). This is a disaster recovery async member. All its mirrored databases are read-only mirrored databases.

- 1 - Read-Only Reporting. This is a reporting async member. All its mirrored databases could be read-only or read-write databases. The default is read-only when the database is created.

- 2 - Read-Write Reporting. This is a reporting async member. All its mirrored databases could be read-only or readwrite databases. The default is read-write when the database is created.

For more information, see Async Mirror Members.

On the Join as Async page of the Management Portal (System Administration > Configuration > Mirror Settings > Join as Async), fill in Mirror Information and select Next. On the page Async Member Information, in the Async Member System Type row, select a type from the drop-down list.

Instead of using the Management Portal, you can change AsyncMemberType in the Config.MirrorMember class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

AsyncUseSystemPurgeInterval

AsyncUseSystemPurgeInterval

Specify how mirror journal files are pur ged on the reporting async member.

Synopsis

[MirrorMember] AsyncUseSystemPurgeInterval=n

n is either 1 or 0.

Description
AsyncMemberType indicates how the reporting async member purges mirror journal files recei ved from the primary failover member.

- 0 - Mirror journal files are pur ged immediately after being dejournaled.

- 1- Mirror journal files are pur ged according to the instances journal file pur ge criteria.

For more information, see Editing or Removing an Async Members.

On the Edit Async page of the Management Portal (System Administration > Configuration > Mirror Settings > Edit Async), for a reporting async member, use the Mirror Journal File Retention drop-down to determine how mirror journal files are purged.

Instead of using the Management Portal, you can change AsyncUseSystemPurgeInterval in the Config.MirrorMember class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[MirrorMember]

JoinMirror

Specify whether the instance processes its mirror configuration at startup.

Synopsis

[MirrorMember] JoinMirror=n

n is either 1 or 0.

Description
When JoinMirror is enabled (n = 1), the mirror configuration is processed and the instance is considered a mirror member according to its configuration.

When this parameter is not enabled, the mirror configuration is ignored and the instance is not initialized as a mirror member. This is recommended when there is a problem in the configuration which pre vents the instance from starting, or if the member must be reconfigured before joining the mirror . For example, if an instance was the primary but no longer is, prevents the system from joining the mirror when it restarts and attempting to become the primary again, which could result in dual primaries.

You can change JoinMirror in the Config.MirrorMember class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

SystemName

SystemName

Set the name of the mirror member.

Synopsis

[MirrorMember] SystemName=Name

n is a string with a maximum length of 32 characters.

Description
SystemName is the name for the failover member you are configuring on this instance. This defaults to a combination of the system host name and the InterSystems IRIS® data platform instance name.

Mirror member names are converted to uppercase before storing. They cannot contain spaces, tabs, or the any of following characters

: [ ] # ; / * = ^ ~ ,

On any of the Create a Mirror, Join as Failover, or Join as Async pages of the Management Portal (System Administration > Configuration > Mirror Settings), enter the Mirror Member Name.

Instead of using the Management Portal, you can change SystemName in the Config.MirrorMember class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[MirrorMember]

ValidatedMember

Specify whether the instance should join the mirror in its previous role or obtain its new role from the current primary before joining the mirror.

Synopsis

[MirrorMember] ValidatedMember=n

n is either 1 or 0.

Description
When ValidatedMember is enabled (n = 1), the instance joins the mirror in its current role.

When this parameter is not enabled, the instance contacts the primary to obtain its current role before joining the mirror. Use this when there have been role changes within the mirror while the instance and its ISCAgent were down or unreachable. For example, if a DR async has been promoted to backup while the former backup was down, set ValidatedMember to 0 before restarting the instance to ensure that the former backup receives its new role of DR async from the primary before restarting the mirror.

You can change ValidatedMember in the Config.MirrorMember class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

VirtualAddressInterface

VirtualAddressInterface

When a virtual IP address is configured, this is the name of the local netw ork interface hosting the IP address.

Synopsis

[MirrorMember] VirtualAddressInterface=InterfaceName

Description
A mirror virtual IP address (VIP) allows all external clients (language bindings, ODBC/JDBC/SQL clients, and so on) to connect to the mirror through a single address. For more information, see the [Mirrors] section and Configuring a Mirror Virtual IP (VIP).

On the Create a Mirror page of the Management Portal (System Administration > Configuration > Mirror Settings > Create a Mirror) select Use Virtual IP. Select the Network Interface you wish to use.

Instead of using the Management Portal, you can change VirtualAddressInterface in the Config.MirrorMember class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[Mirrors]

This topic describes the [Mirrors] section of the CPF.

[Mirrors]

Mirrors

Define mirrors that include this instance.

Synopsis

[Mirrors] Name=a,b,c,d,e,f,g,h,i,j|k,l,m,n,o,p

Description
The [Mirrors] section contains an entry for each mirror connected to the current InterSystems IRIS® data platform
instance. The entries are made up of a Name and 14 comma-separated values (a—n), as follows:

- Name – Required. Unique name by which you can identify this mirror. An uppercase alphanumeric string with a maximum length of 15 characters.

- a (GUID) – Required. An internal GUID, unique to this mirror.

- b (2) – For internal use.

- c (1) – For internal use.

- d (Defined Primary) – Used to disable mirror f ailover, generally for maintenance purposes. Manipulated via the nofailover option when shutting down InterSystems IRIS or the ^MIRROR routine. This contains the mirror name of the member which must be the primary. It is cleared automatically when that node starts up and becomes the primary.

- e (QOSTimeout) – Quality of Service Timeout: the maximum time, in milliseconds, that a failover member waits for
a response from the other failover member before taking action; also applies to the arbiter’s wait for a failover member’s
response. The default is 8000ms; typically, deployments on physical (non-virtualized) hosts with a dedicated local
network can reduce this setting if a faster response to outages is required. See Configuring the Quality of Service (QoS) Timeout Setting for more information on the QoS Timeout setting.

- f (0) – For internal use.

- g (UseSSL) – To provide security within a mirror, you can configure its nodes to use SSL/TLS. This provides for both authentication of one node to another, and for encrypted communication between nodes. To use SSL/TLS with a mirror, each member (failover or async) uses a pair of SSL/TLS configurations, %Mirror_Client and %Mirror_Serv er. These configurations must already e xist on each member when SSL/TLS is enabled for the mirror. Instructions for setting up SSL are in Create and Edit TLS Configurations for a Mirror . Values are 0 (no, default) or 1 (yes).

- h (VirtualAddress) – Specifies a virtual IP address. You can configure a mirror virtual IP address (VIP) so that all external clients (language bindings, ODBC/JDBC/SQL clients, and so on) connect to the mirror through a single address. This virtual IP address is automatically bound to an interface on the current primary member. To use a VIP, both failover members must be on the same subnet. For more information, see Configuring a Mirror Virtual IP (VIP).

- i (0) – For internal use.

- j (ArbiterNode) – The network address of the arbiter configured for this mirror . The arbiter is an independent instance hosting an ISCAgent with which the failover members of a mirror maintain continuous contact, providing them with the context needed to safely make failover decisions when they cannot communicate directly.

- k (ArbiterPort) – The port used by the configured arbiter’ s ISCAgent process (2188 by default). Appears in the same space as j, separated by a vertical bar.

- l (CompressionForFailoverMembers) – Determines whether journal data is compressed before being transmitted from the primary to the backup. Possible values are 0 (System Selected, which optimizes for response time between failover members), 1 (Uncompressed), and 2 (Compressed).

Mirrors

- m (CompressionForAsyncMembers) – Determines whether journal data is compressed before being transmitted from the primary to async members. Possible values are 0 (System Selected, which optimizes for network utilization), 1 (Uncompressed), and 2 (Compressed).

- n (AllowParallelDejournaling) – Determines which type of mirror members can run parallel dejournaling updaters. Possible values are 0 (failover and disaster recover members), 1 (failover members only), and 2 (all members).

- o (CompressionTypeForFailoverMembers) – Determines the compression type for CompressionForFailoverMembers. Possible values are 0 (ZLIB), 1 (ZSTD), and 2 (LZ4).

- p (CompressionTypeForAsyncMembers) – Determines the compression type for CompressionForAsyncMembers. Possible values are 0 (ZLIB), 1 (ZSTD), and 2 (LZ4).

For more information on Mirroring, see Mirroring.

Management Portal
On the Create a Mirror page of the Management Portal (System Administration > Configuration > Mirror Settings > Create a Mirror), enter the requested information. You can edit an existing mirror on the Edit Mirror page, but only from the primary failover member.

[Miscellaneous]

Important:

The [Miscellaneous] parameters have been retained for compatibility, and all except for ShutDownLogErrors should not be used when building new applications.

This topic describes the Compatibility Settings parameters found in the [Miscellaneous] section of the CPF.

[Miscellaneous]

AsyncDisconnectErr

Allow processes to receive disconnect errors asynchronously.

Synopsis

[Miscellaneous] AsyncDisconnectErr=n

n is either 1 (true) or 0 (false). The default value is 0.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

AsyncDisconnectErr modifies the beha vior of InterSystems IRIS® data platform when DisconnectErr is enabled. When AsyncDisconnectErr is enabled (n = 1), the process receives an asynchronous <DSCON> error at the time a disconnect occurs on the device. This error occurs at the next command executed, and interrupts hang commands

When this parameter is not enabled, the process receives a <DSCON> error at the next read or write command.

AsyncDisconnectErr is only applicable to Telnet connections on Windows. It has no effect on any other device type or operating system. If DisconnectErr is set to 0 (false), then AsyncDisconnectErr has no effect.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the AsyncDisconnectErr row, click Edit. Select AsyncDisconnectErr to enable this setting.

Instead of using the Management Portal, you can change AsyncDisconnectErr in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

To change this parameter for a single process only (as opposed to instance-wide), use the AsyncDisconnectErr() method of the %SYSTEM.Process class. See the class reference for details.

AsynchError

AsynchError

Allow processes to receive asynchronous errors.

Synopsis

[Miscellaneous] AsynchError=n

n is either 1 (true) or 0 (false). The default value is 1.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

When AsynchError is enabled (n = 1), InterSystems IRIS® data platform processes can receive asynchronous errors.

If the server detects multiple asynchronous errors for a particular job, the system triggers at least one such error. However,
there is no guarantee which error will be triggered. The possible asynchronous errors include:

- <LOCKLOST> — Some locks once owned by this job have been reset.

- >DATALOST> — Some data modifications performed by this job ha ve received an error from the server.

- <TRANLOST> — The server has asynchronously rolled back a distributed transaction initiated by this job.

Even if you disable a job receiving asynchronous errors, the next time the job performs a ZSync command, the asynchronous error is triggered.

During a transaction, InterSystems IRIS checks for pending asynchronous errors at each transaction start, commit, LOCK operation, and every network global reference. Since SET and KILL operations across the network are asynchronous, other instructions may be executed between the time a SET is generated and when the asynchronous error is reported.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the AsynchError row, click Edit. Select AsynchError to enable this setting.

Instead of using the Management Portal, you can change AsynchError in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

To change this parameter for a single process only (as opposed to instance-wide), use the AsynchError() method of the %SYSTEM.Process class. See the class reference for details.

[Miscellaneous]

BreakMode

Specify programmer mode response to the BREAK command.

Synopsis

[Miscellaneous] BreakMode=n

n is either 1 (true) or 0 (false). The default value is 1.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

InterSystems IRIS® data platform programs can execute in two modes, depending on how InterSystems IRIS is entered:
application mode and programmer mode.

BreakMode controls how an InterSystems IRIS process in programmer mode responds when it encounters a BREAK command that has no argument. When BreakMode is enabled (n = 1), InterSystems IRIS enters the debugger or returns to the direct mode prompt with a <BREAK> error. When this parameter is not enabled, the BREAK command is ignored.

Application mode jobs always ignore argumentless BREAK commands.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the BreakMode row, click Edit. Select BreakMode to enable this setting.

Instead of using the Management Portal, you can change BreakMode in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

To change this parameter for a single process only (as opposed to instance-wide), use the BreakMode() method of the %SYSTEM.Process class. See the class reference for details.

CollectResourceStats

CollectResourceStats

Allow InterSystems IRIS® data platform to collect instance resource statistics.

Synopsis

[Miscellaneous] CollectResourceStats=n

n is either 1 (true) or 0 (false). The default value is 0.

Description

Important:

This parameter has been retained for compatibility, but has no effect. Resource statistics are always collected by all instances on all supported platforms.

When CollectResourceStats is enabled (n = 1), InterSystems IRIS collects instance resource statistics (seize, nseize, aseize, bseize).

For more information on instance resource statistics, see Monitoring Performance Using ^mgstat and the Enumresource function in Monitoring InterSystems IRIS Using Web Services.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the CollectResourceStats row, click Edit. Select CollectResourceStats to enable this setting.

Instead of using the Management Portal, you can change CollectResourceStats in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

[Miscellaneous]

DisconnectErr

Specify how processes respond to a disconnect.

Synopsis

[Miscellaneous] DisconnectErr=n

n is either 1 (true) or 0 (false). The default value is 0.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

DisconnectErr determines how InterSystems IRIS® data platform responds to a disconnect of the principal I/O device. When the parameter is enabled, the process receives a <DSCON> error when a disconnect is detected during an ObjectScript Write or Read command. When the parameter is not enabled, the process exits without reporting an error to the application when a disconnect is detected.

If DisconnectErr is enabled, a process continues to execute after its principal device has been disconnected. It is the responsibility of the application to detect the <DSCON> error and exit gracefully. Use care when enabling DisconnectErr.

DisconnectErr is only applicable to TCP devices and to terminal devices where a disconnect can be recognized. Examples are modem controlled terminals and Windows Telnet, and Windows local iristerm (TRM:) connections. DisconnectErr is only applicable to the principal device.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the DisconnectErr row, click Edit. Select DisconnectErr to enable this setting.

Instead of using the Management Portal, you can change DisconnectErr in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

To change this parameter for a single process only (as opposed to instance-wide), use the DisconnectErr() method of the %SYSTEM.Process class. See the class reference for details.

FileMode

FileMode

Allow writing to a non-existent file.

Synopsis

[Miscellaneous] FileMode=n

n is either 1 (true) or 0 (false). The default value is 0.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

When FileMode is enabled (n = 1), if a file that does not e xist is opened for reading or writing, a new file is created. When this parameter is not enabled, a new file is not created (unless specified in the OPEN command parameters).

Suppose InterSystems IRIS® data platform encounters an OPEN command such as:

OPEN "file.x":"WS"

When FileMode=1 the new file is created automatically , even though the “N” parameter is not specified with the OPEN command. The result when FileMode=1 is equivalent to adding the N parameter to each OPEN command, so that the
above OPEN command is equivalent to:

OPEN "file.x":"WNS"

On the other hand, when InterSystems IRIS encounters an OPEN command and no N parameter is provided and the file does not already exist, then if FileMode=0 there is no result from the OPEN command except that the process hangs until interrupted.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the FileMode row, click Edit. Select FileMode to enable this setting.

Instead of using the Management Portal, you can change FileMode in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

To change this parameter for a single process only (as opposed to instance-wide), use the FileMode() method of the %SYSTEM.Process class. See the class reference for details.

[Miscellaneous]

GlobalKillEnabled

Allow KILL of an unsubscripted global.

Synopsis

[Miscellaneous] GlobalKillEnabled=n

n is either 1 (true) or 0 (false). The default value is 1.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

Deprecated. When GlobalKillEnabled is enabled (n = 1), a KILL of an unsubscripted global is allowed, so you can kill all subscripts of a global with a single kill instead if killing them individually. When this parameter is not enabled, the KILL results in a <PROTECT> error

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the GlobalKillEnabled row, click Edit. Select GlobalKillEnabled to enable this setting.

Instead of using the Management Portal, you can change GlobalKillEnabled in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

To change this parameter for a single process only (as opposed to instance-wide), use the GlobalKillDisabled() method of the %SYSTEM.Process class. Note that this is the inverse of GlobalKillEnabled. See the class reference for details.

IEEEError

IEEEError

Specify whether $DOUBLE returns INF and NAN values instance-wide.

Synopsis

[Miscellaneous] IEEEError=n

n is either 1 (true) or 0 (false). The default value is 1.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

IEEEError sets the $DOUBLE function return-value behavior instance-wide. When IEEEError is enabled (n = 1),
$DOUBLE generates InterSystems IRIS® data platform errors for unresolvable IEEE floating point con versions. When
this parameter is not enabled, $DOUBLE returns INF (infinity), -INF , and NAN (Not A Number) for unresolvable IEEE
floating point con versions.

The parameter controls the issuing of INF, -INF, and NAN when a $DOUBLE numeric operation cannot be resolved to a
numeric value. It does not control the issuing of INF, -INF, and NAN in all cases. $DOUBLE always returns INF, -INF,
or NAN when you supply one of these strings as the input value, regardless of this property. Mathematical operations on
$DOUBLE numbers that result in an INF, -INF, or NAN are controlled by this property. These include arithmetic operations,
exponentiation, and logarithmic and trigonometric functions.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the IEEEError row, click Edit. Select IEEEError to enable this setting.

Instead of using the Management Portal, you can change IEEEError in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

To change this parameter for a single process only (as opposed to instance-wide), use the IEEEError() method of the %SYSTEM.Process class. See the class reference for details.

[Miscellaneous]

LicenseAltHeaders

Use an alternative set of HTTP headers for client addresses.

Synopsis

[Miscellaneous] LicenseAltHeaders=n

n is either 1 (true) or 0 (false). The default value is 0.

Description
For client IP addresses, by default, InterSystems IRIS uses the remote_addr HTTP header. If LicenseAltHeaders is true, then InterSystems IRIS instead uses the following headers (as needed, in the order given): HTTP_FORWARDED, or HTTP_X_FORWARDED_FOR, or REMOTE_ADDR.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the LicenseAltHeaders row, click Edit. Select LicenseAltHeaders to enable this setting.

Instead of using the Management Portal, you can change LicenseAltHeaders in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

LineRecall

LineRecall

Allow command line recall for READ commands.

Synopsis

[Miscellaneous] LineRecall=n

n is either 1 (true) or 0 (false). The default value is 1.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

When LineRecall is enabled (n = 1), both READ commands and command prompts can use the line recall feature. When not enabled, only command prompts can use line recall.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the LineRecall row, click Edit. Select LineRecall to enable this setting.

Instead of using the Management Portal, you can change LineRecall in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

To change this parameter for a single process only (as opposed to instance-wide), use the LineRecall() method of the %SYSTEM.Process class. See the class reference for details.

[Miscellaneous]

ListFormat

Specify the compression format for values in a list.

Synopsis

[Miscellaneous] ListFormat=n

n is an integer in the range 0—3. The default value is 1.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

ListFormat determines which values should be compressed within a list. The possible options for ListFormat are:

- 0 — no compression in a list

- 1 — $DOUBLE (IEEE) values in a list are compressed

- 2 — Unicode strings in a list are compressed

- 3 — Both $DOUBLE and Unicode strings in a list are compressed

Note:

If using lists with external clients (Java, C#, etc), ensure that the external client supports the compressed list format.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the ListFormat row, click Edit. Enter the desired value for this setting.

Instead of using the Management Portal, you can change ListFormat in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

- $DOUBLE function

- $LIST function

LogRollback

LogRollback

Allow logging for transaction rollbacks.

Synopsis

[Miscellaneous] LogRollback=n

n is either 1 (true) or 0 (false). The default value is 0.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

When LogRollback is enabled (n = 1), InterSystems IRIS® data platform logs transaction rollbacks to the messages.log file (located in the LogRollback is not enabled, transaction rollbacks are not logged.

install-dir\mgr directory, or the alternate directory named by the console parameter). When

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the LogRollback row, click Edit. Select LogRollback to enable this setting.

Instead of using the Management Portal, you can change LogRollback in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

To change this parameter for a single process only (as opposed to instance-wide), use the LogRollback() method of the %SYSTEM.Process class. See the class reference for details.

[Miscellaneous]

MVDefined

Not in use.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

Not in use.

NodeNameInPid

Specify behavior when InterSystems IRIS® data platform references the special variable $JOB.

NodeNameInPid

Synopsis

[Miscellaneous] NodeNameInPid=n

n is either 1 (true) or 0 (false). The default value is 0.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

When NodeNameInPid is enabled (n = 1), $JOB returns the process ID number of the current process concatenated to
the node name. When NodeNameInPid is not enabled, $JOB returns only the process ID number, without the node name.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the NodeNameInPid row, click Edit. Select NodeNameInPid to enable this setting.

Instead of using the Management Portal, you can change NodeNameInPid in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

To change this parameter for a single process only (as opposed to instance-wide), use the NodeNameInPid() method of the %SYSTEM.Process class. See the class reference for details.

[Miscellaneous]

NullSubscripts

Allow null subscripts on global references.

Synopsis

[Miscellaneous] NullSubscripts=n

n is either 1 (true) or 0 (false). The default value is 0.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

When NullSubscripts is enabled (n = 1), null subscripts are allowed on global references. When this parameter is not enabled, a null subscript causes a <SUBSCRIPT> error. InterSystems recommends leaving this setting disabled.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the NullSubscripts row, click Edit. Select NullSubscripts to enable this setting.

Instead of using the Management Portal, you can change NullSubscripts in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

To change this parameter for a single process only (as opposed to instance-wide), use the NullSubscripts() method of the %SYSTEM.Process class. See the class reference for details.

OldZU5

OldZU5

Specify whether to clear global vectors when switching namespace.

Synopsis

[Miscellaneous] OldZU5=n

n is either 1 (true) or 0 (false). The default value is 0.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

When OldZU5 is enabled (n = 1), switching to the current namespace using the ZN command clears the global vector cache. When this parameter is not enabled, switching to the current namespace has no effect.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the OldZU5 row, click Edit. Select OldZU5 to enable this setting.

Instead of using the Management Portal, you can change OldZU5 in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

To change this parameter for a single process only (as opposed to instance-wide), use the OldZU5() method of the %SYSTEM.Process class. See the class reference for details.

[Miscellaneous]

OpenMode

Specify read/write mode to use when opening sequential files.

Synopsis

[Miscellaneous] OpenMode=n

n is either 1 (true) or 0 (false). The default value is 0.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

OpenMode specifies the def ault read/write mode to use when opening sequential files with the OPEN command. The options are Read-Write (1) or Read (0).

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the OpenMode row, click Edit. Choose a mode, Read (0) or Read-Write (1).

Instead of using the Management Portal, you can change OpenMode in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

To change this parameter for a single process only (as opposed to instance-wide), use the OpenMode() method of the %SYSTEM.Process class. See the class reference for details.

PopError

PopError

Specify when to pop error handlers off the stack.

Synopsis

[Miscellaneous] PopError=n

n is either 1 (true) or 0 (false). The default value is 0.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

When PopError is enabled (n = 1), InterSystems IRIS® data platform pop the $ZTRAP error handler off the stack when
an error is triggered. In this case, when a $ZTRAP error handler is invoked by the instance, the error handler is removed
from the stack. Thus, if an error occurs while the error handler is executing, that error is handled by the previous error handler on the stack.

When this parameter is not enabled, the normal behavior prevails: A $ZTRAP error handler stays active when the error
handler is invoked. In this case, when a $ZTRAP error handler is invoked by the instance, that error handler remains on
the stack of established error handlers. Thus, if an error occurs when the error handler is executing, that error handler attempts to invoke itself, receives the same error again, and enters an infinite loop, unless that error handler e xplicitly sets
$ZTRAP to a new value.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the PopError row, click Edit. Select PopError to enable this setting.

Instead of using the Management Portal, you can change PopError in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

To change this parameter for a single process only (as opposed to instance-wide), use the PopError() method of the %SYSTEM.Process class. See the class reference for details.

[Miscellaneous]

RefInKind

Specify how $NAME and $QUERY handle extended global references.

Synopsis

[Miscellaneous] RefInKind=n

n is either 1 (true) or 0 (false). The default value is 0.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

When RefInKind is enabled (n = 1), given an input that is an extended global reference, $NAME and $QUERY return
only the global name without the extended reference. When this parameter is not enabled, the functions return an extended global reference.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the RefInKind row, click Edit. Select RefInKind to enable this setting.

Instead of using the Management Portal, you can change RefInKind in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

Note:

If you change the value of this parameter, the change applies to processes started after the change, but not for processes that were already running when you made the change.

To change this parameter for a single process only (as opposed to instance-wide), use the RefInKind() method of the %SYSTEM.Process class. See the class reference for details.

ScientificNotation

ScientificNotation

Allow lowercase “e ” as scientific notation symbol instance-wide.

Synopsis

[Miscellaneous] ScientificNotation=n

n is either 1 (true) or 0 (false). The default value is 1.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

When ScientificNotation is enabled (n = 1), InterSystems IRIS® data platform uses the lowercase “e” as scientific notation symbol instance-wide.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the ScientificNotation row, click Edit. Select ScientificNotation to enable this setting.

Instead of using the Management Portal, you can change ScientificNotation in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

To change this parameter for a single process only (as opposed to instance-wide), use the ScientificNotation() method of the %SYSTEM.Process class. See the class reference for details.

[Miscellaneous]

SetZEOF

Specify the behavior when reading a sequential file and encountering an une xpected end-of-file error .

Synopsis

[Miscellaneous] SetZEOF=n

n is either 1 (true) or 0 (false). The default value is 0.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

When SetZEOF is enabled (n = 1), InterSystems IRIS® data platform sets the special variable $ZEOF to indicate that you
have reached the end of a sequential file. When this parameter is not enabled, InterSystems IRIS throws an <ENDOFFILE> error instead

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the SetZEOF row, click Edit. Select SetZEOF to enable this setting.

Instead of using the Management Portal, you can change SetZEOF in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

To change this parameter for a single process only (as opposed to instance-wide), use the SetZEOF() method of the %SYSTEM.Process class. See the class reference for details.

ShutDownLogErrors

Allow writing of InterSystems IRIS® data platform system error log entries to the messages.log file on shutdo wn.

ShutDownLogErrors

Synopsis

[Miscellaneous] ShutDownLogErrors=n

n is either 1 (true) or 0 (false). The default value is 0.

Description
When ShutDownLogErrors is enabled (n = 1), during shutdown InterSystems IRIS logs error information from ^SYSLOG to the messages.log file (located in the install-dir\mgr directory, or the alternate directory named by the console parameter). When ShutDownLogErrors is not enabled, these errors are not logged.

For more information, see InterSystems IRIS System Error Log.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the ShutDownLogErrors row, click Edit. Select ShutDownLogErrors to enable this setting.

Instead of using the Management Portal, you can change ShutDownLogErrors in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

[Miscellaneous]

StopID

Not in use.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

Not in use.

SwitchOSdir

SwitchOSdir

Disallow switching current working directories when changing namespaces.

Synopsis

[Miscellaneous] SwitchOSdir=n

n is either 1 (true) or 0 (false). The default value is 0.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

SwitchOSdir specifies what happens to the current w orking directory (for accessing files by relati ve pathname, etc.) when you switch to a different namespace. When SwitchOSdir is enabled (n = 1), if you change namespaces, the current working directory remains unaltered no matter what namespace you switch to.

When this parameter is not enabled, if you change namespaces, the current working directory is changed to the directory of the default dataset for non-% globals of the new namespace. However, if this dataset is remote (networked to a different system), the current working directory is left unchanged.

For example, suppose SwitchOSdir is set to 1, or SwitchOSdir is set to 0 and the dataset is remote. In these cases, the current working directory does not change automatically as a result of changing the namespace, but you can always change the current working directory programmatically.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the SwitchOSDir row, click Edit. Select SwitchOSDir to enable this setting.

Instead of using the Management Portal, you can change SwitchOSDir in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

To change this parameter for a single process only (as opposed to instance-wide), use the SwitchOSDir() method of the %SYSTEM.Process class. See the class reference for details.

[Miscellaneous]

SynchCommit

Disable synchronizing TCOMMIT with the corresponding journal write operation.

Synopsis

[Miscellaneous] SynchCommit=n

n is either 1 (true) or 0 (false). The default value is 0.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

Every TCOMMIT command requests a flush of the journal data in volved in that transaction to disk. SynchCommit controls what happens at that point. When enabled (n = 1), TCOMMIT completes after the journal data write operation completes. If SynchCommit is not enabled, TCOMMIT completes without waiting for the write operation.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the SynchCommit row, click Edit. Select SynchCommit to enable this setting.

Instead of using the Management Portal, you can change SynchCommit in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

To change this parameter for a single process only (as opposed to instance-wide), use the SynchCommit() method of the %SYSTEM.Process class. See the class reference for details.

TelnetNUL

TelnetNUL

Suppress Telnet NUL at end-of-line for Telnet transmission. Windows systems only.

Synopsis

[Miscellaneous] TelnetNUL=n

n is either 1 (true) or 0 (false). The default value is 0.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

On output, a Telnet network virtual terminal (NVT) performs the following default end-of-line behavior: either issues a carriage return character (CR) followed by a linefeed character (LF), or issues a CR followed by NUL (if no LF is issued). TelnetNUL affects the issuance of the NUL character in the second case. When TelnetNul is enabled (n = 1), the Telnet virtual terminal suppresses the NUL character.

This setting applies only to Windows systems; it is ignored on UNIX®, and Linux configurations, in which Telnet is supplied
by the operating system vendor.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the TelnetNUL row, click Edit. Select TelnetNUL to enable this setting.

Instead of using the Management Portal, you can change TelnetNUL in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor

To change this parameter for a single process only (as opposed to instance-wide), use the TelnetNUL() method of the %SYSTEM.Process class. See the class reference for details.

[Miscellaneous]

TruncateOverflow

Suppress the <MAXNUMBER> error on numeric overflo w.

Synopsis

[Miscellaneous] TruncateOverflow=n

n is either 1 (true) or 0 (false). The default value is 0.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

Normally, when InterSystems IRIS® data platform encounters an extremely large number (on the order of 1.0E147, or - 1.0E146), it throws the <MAXNUMBER> error. When TruncateOverflow is enabled, the <MAXNUMBER> error is suppressed.

For more information, see Extremely Large Numbers.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the TruncateOverflow row, click Edit. Select TruncateOverflow to enable this setting.

Instead of using the Management Portal, you can change TruncateOverflow in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor

To change this parameter for a single process only (as opposed to instance-wide), use the TruncateOverflo w() method of the %SYSTEM.Process class. See the class reference for details.

- $DOUBLE function

Undefined

Undefined

Specify the response when ObjectScript attempts to fetch a variable that does not exist.

Synopsis

[Miscellaneous] Undefined=n

n is either 0, 1, or 2. The default value is 0.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

The parameter Undefined specifies the beha vior when ObjectScript attempts to fetch the value of a variable that has not
been defined. The value of Undefined may be 0, 1, or 2:

- 0 - Always throw an <UNDEFINED> error. (default)

- 1 - If the undefined v ariable has subscripts, return a null string, but if the undefined v ariable is single-valued, throw an <UNDEFINED> error.

- 2 - Always return a null string.

On the Compatibility page Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the Undefined row, click Edit. Select the option you want.

Instead of using the Management Portal, you can change Undefined in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor

To change this parameter for a single process only (as opposed to instance-wide), use the Undefined() method of the %SYSTEM.Process class. See the class reference for details.

[Miscellaneous]

UseNagleAlgorithm

Allow InterSystems IRIS® data platform to use the Nagle algorithm for Telnet.

Synopsis

[Miscellaneous] UseNagleAlgorithm=n

n is either 1 (true) or 0 (false). The default value is 0.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

When UseNagleAlgorithm is enabled (n = 1), InterSystems IRIS uses the Nagle algorithm for Telnet.

The Nagle algorithm makes Telnet more efficient. It reduces the number of IP pack ets sent over the network by consolidating messages that are sent within a small time interval into a single IP packet. When the Nagle algorithm is enabled, the operating system waits some interval before actually committing the data from a send command, in the hopes that the application calls send again with more data that can be consolidated with the first. F or more details see RFC 896.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the UseNagleAlgorithm row, click Edit. Select UseNagleAlgorithm to enable this setting.

Instead of using the Management Portal, you can change UseNagleAlgorithm in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

ViewPastData

ViewPastData

Allow $VIEW to examine data outside of InterSystems IRIS® data platform memory area.

Synopsis

[Miscellaneous] ViewPastData=n

n is either 1 (true) or 0 (false). The default value is 0.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

When ViewPastData is enabled (n = 1), you can use the $VIEW command to examine data outside of InterSystems IRIS
memory area. When this parameter is not enabled, the $VIEW command throws an error.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the ViewPastData row, click Edit. Select ViewPastData to enable this setting.

Instead of using the Management Portal, you can change ViewPastData in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

[Miscellaneous]

ZDateNull

Specify the $ZDATE response to an invalid value.

Synopsis

[Miscellaneous] ZDateNull=n

n is either 1 (true) or 0 (false). The default value is 0.

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

ZDateNull determines how a $ZDATE call responds when triggered by an invalid value. When this parameter is enabled,
$ZDATE returns a null value. When this parameter is not enabled, it returns an error.

On the Compatibility page of the Management Portal (System Administration > Configuration > Additional Settings > Compatibility), in the ZDateNull row, click Edit. Select ZDateNull to enable this setting.

Instead of using the Management Portal, you can change ZDateNull in the Config.Miscellanous class (as described in the class reference) or by editing the CPF with a text editor.

To change this parameter for a single process only (as opposed to instance-wide), use the ZDateNull() method of the %SYSTEM.Process class. See the class reference for details.

[Monitor]

This topic describes the parameters found in the [Monitor] section of the CPF.

[Monitor]

SNMPEnabled

Allow automatic Simple Network Management Protocol (SNMP) startup.

Synopsis

[Monitor] SNMPEnabled=n

n is either 1 or 0. The default value is 0.

Description
When SNMPEnabled is enabled (n = 1), the SNMP agent automatically starts when InterSystems IRIS® data platform starts up.

To enable SNMP monitoring, select Start SNMP Agent at System Startup. You must also have the %Service_Monitor enabled on the Services page (System Administration > Security > Services).

On the Monitor page of the Management Portal (System Administration > Configuration > Additional Settings > Monitor), select Start SNMP Agent at System Startup to enable this setting. There is also a shortcut to enable %Service_Monitor.

Instead of using the Management Portal, you can change SNMPEnabled in the Config.Monitor class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

- Monitoring InterSystems IRIS Using SNMP.

OTELLogs

OTELLogs

Enable the export of log events to an OpenTelemetry-compatible monitoring tool.

Synopsis

[Monitor] OTELLogs=n

n is either 1 or 0. The default value is 0.

Description
When OTELLogs is enabled (n = 1), the OpenTelemetry exporter agent automatically starts when InterSystems IRIS® data platform starts. The exporter emits log events from the system messages log and the audit database to the designated OTLP/HTTP endpoint.

The exporter emits only log events which meet or exceed the severity level which is specified by the OTELLogLevel parameter. It emits new log events at regular intervals, as specified by OTELInterval.

On the Monitor page of the Management Portal (System Administration > Configuration > Additional Settings > Monitor), select Enable OTel Logs to enable this setting.

Instead of using the Management Portal, you can change OTELLogs in the Config.Monitor class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

- [Monitor]

OTELMetrics

Enable the export of metrics to an OpenTelemetry-compatible monitoring tool.

Synopsis

[Monitor] OTELMetrics=n

n is either 1 or 0. The default value is 0.

Description
When OTELMetrics is enabled (n = 1), the OpenTelemetry exporter agent automatically starts when InterSystems IRIS® data platform starts up. The exporter emits all the metric events which the /api/monitor API records to the designated OTLP/HTTP endpoint.

The exporter emits these metric events at regular intervals, as specified by OTELInterval.

On the Monitor page of the Management Portal (System Administration > Configuration > Additional Settings > Monitor), select Enable OTel Metrics to enable this setting.

Instead of using the Management Portal, you can change OTELMetrics in the Config.Monitor class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

- OTELInterval Set the regular interval at which metric or log events are exported to an OpenTelemetry-compatible monitoring tool.

OTELInterval

Synopsis

[Monitor] OTELInterval=n

n is an integer in the range 1–65,636 (representing a number of seconds). The default value is 10.

Description
When either OTELLogs or OTELMetrics is enabled, OTELInterval determines the regular interval at which InterSystems IRIS® data platform emits log or metric events to the designated OTLP/HTTP endpoint.

On the Monitor page of the Management Portal (System Administration > Configuration > Additional Settings > Monitor), edit the OTel Exporter Interval field.

Instead of using the Management Portal, you can change OTELInterval in the Config.Monitor class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

- [Monitor]

OTELLogLevel

Set the threshold severity level to determine which log events are exported to an OpenTelemetry-compatible monitoring tool

Synopsis

[Monitor] OTELLogLevel=level

level is a string (one of the recognized severity levels for structured logging events). The default value is WARN.

Description
When OTELLogs is enabled, InterSystems IRIS® data platform emits a log event to the designated OTLP/HTTP endpoint if the severity level of the log event matches or exceeds the severity level identified by OTELLogLevel. The exporter does not emit log events which have a severity level lower than the level identified by OTELLogLevel.

On the Monitor page of the Management Portal (System Administration > Configuration > Additional Settings > Monitor), select the desired value from the OTel Log Level drop-down menu.

Instead of using the Management Portal, you can change OTELLogLevel in the Config.Monitor class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

- [Namespaces] This topic describes the parameters found in the [Namespaces] section of the CPF.

[Namespaces]

Namespace

Define InterSystems IRIS® data platform namespaces.

Synopsis

[Namespaces] Name=globals,routines,temporary

Description
The [Namespaces] section contains an entry for every namespace defined for the InterSystems IRIS instance. InterSystems IRIS adds entries to the configuration parameter file automatically as you add and configure namespaces using the Management Portal.

Each entry contains up to three comma-separated values, but only the first v alue is required. If the other values are not
specified, the y are set to the instance default. The values are as follows:

- globals — Default database name for globals (other than temporary globals). Required.

- routines — Default database name for routines and classes. If the database is not specified, it def aults to the globals database.

- temporary — Default database name for temporary storage, specifically storage of temporary globals. If the database is not specified, it def aults to IRISTEMP.

A temporary global is a global whose name starts with ^IRIS.Temp (case-sensitive).

Exceptions to Defaults

Globals starting with a % are mapped to IRISSYS unless mapped to another database by a user-defined global mapping.

In all namespaces, routine and classes that start with a % come from the IRISLIB database with the following exceptions:

- Routines and classes starting with %SYS.* come from IRISSYS (supplied by InterSystems IRIS).

- Routines and classes starting with %Z* and %z* come from IRISSYS (user defined routines and classes).

- Routines and classes that are explicitly mapped from another database by the user using routine or package mapping.

Note that routines that reside in the IRISSYS database have special security privileges including the ability to modify the roles and other security attributes of the process executing them.

Examples

In the [Namespaces] section, each entry appears all on one line:

[Namespaces]
%SYS=IRISSYS
USER=USER
SALES=SALESGBL,SALESRTN
; Globals and routines/classes split into separate databases.
BILLING=BILLING,,TEMPDATA
; Globals and routines/classes in the same database,
; temporary globals are mapped to the databases TEMPDATA

On the Namespaces page of the Management Portal (System Administration > Configuration > System Configuration > Namespaces), to add a new entry, select Create New Namespace. To edit an existing entry, select Edit in that entry's row.

The Global entry in the [Map] section of this book.

The Package entry in the [Map] section of this book.

The Routine entry in the [Map] section of this book.

Namespace

[SQL]

This topic describes the settings on the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL). The bottom of this page contains a list of parameters found in the [SQL] section of the CPF.

The SQL page is divided into the SQL tab and the SQL Shell tab . The SQL tab allows you to configure v arious SQL settings, which correspond to CPF parameters as shown in the table below. The SQL Shell tab options are described in Configuring the SQL Shell.

Table X–1: SQL Tab

SQL Tab Setting

Equivalent CPF Parameter

Retain cached query source

SaveMAC

Default time precision for GETDATE(), CURRENT_TIME, and

TimePrecision

CURRENT_TIMESTAMP

Lock escalation threshold

TO_DATE default format

LockThreshold

TODATEDefaultFormat

Disable automatic statistics collection for tables with fixed

AutoStatsForFixedStatsTable

statistics.

Enable automatic statistics collection for tables whose

AutoStatsForRemoteGlobalTable

default data global maps to remote databases.

Disable automatic statistics collection for tables supporting

AutoStatsForEfficientSamplingTableOnly

efficient sampling only.

Default length for VARCHAR

Default schema

Execute queries in a single process

ODBCVarcharMaxlen

DefaultSchema

AutoParallel

Define primary key as ID key for tables created via DDL

IdKey

Ignore redundant DDL statements

Sets the DDLNo* parameters.

GROUP BY and DISTINCT queries must produce original

FastDistinct

values

Turn off Adaptive Mode to disable run time plan choice and

AdaptiveMode

automatic tuning

Turn on parameter sampling to sample the parameter value

ParameterSampling

for query execution

Lock timeout (seconds)

TCP keepalive for client connections (seconds)

LockTimeout

TCPKeepAlive

Client maximum idle time (seconds)

ClientMaxIdleTime

[SQL]

ANSIPrecedence

Specify operator precedence for SQL queries.

Synopsis

[SQL] ANSIPrecedence=n

n is either 1 or 0. The default value is 1.

Description
When ANSIPrecedence is enabled (n = 1), InterSystems SQL uses ANSI precedence of arithmetic operators. If ANSIPrecedence is disabled (n = 0), InterSystems SQL executes arithmetic expressions in strict left-to-right order. This is an instance-wide configuration setting.

When ANSI precedence is configured, the “*”, “\”, “/ ”, and “#” operators have a higher precedence than the “+ ”, “-”, and “||” operators. Operators with a higher precedence are executed before operators with a lower precedence. You can use parentheses to override precedence when desired.

For further details, see SQL Operator Precedence.

To set the desired value for ANSIPrecedence from the Terminal, use the SetOption(“ANSIPrecedence”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change ANSIPrecedence with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

- Arithmetic Operators and Functions

AdaptiveMode

AdaptiveMode

Enable Adaptive Mode performance optimization options.

Synopsis

[SQL] AdaptiveMode=n

n is either 1 or 0. The default value is 1.

Description
When AdaptiveMode is enabled (n = 1), InterSystems SQL uses Adaptive Mode, which enables automation and adaptive
settings whenever possible. Adaptive Mode configures the follo wing options:

- Disables freezing of existing query plans when you upgrade InterSystems IRIS® data platform to a new major version. Upon upgrade, non-frozen query plans are invalidated and replanned when called for the first time after upgrade. F or more details, see Frozen Plans.

- Enables automatic table tuning when an untuned table is first queried. With table tuning, InterSystems SQL gathers statistics from a table that it can use to optimize future queries. For more details, see Table Statistics for Query Optimizer.

- Enables Runtime Plan Choice. In doing so, InterSystems SQL takes the runtime values of query parameters into account to consider alternative plans, including in the presence of outliers. As a result, enabling this parameter overrides the older BiasQueriesAsOutlier and RTPC settings.

On the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL), select Turn off Adaptive Mode to disable run time plan choice and automatic tuning to disable Adaptive Mode.

You can also change AdaptiveMode with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[SQL]

AllowRowIDUpdate

Allow user to update RowID values.

Synopsis

[SQL] AllowRowIDUpdate=n

n is either 1 or 0. The default value is 0.

Description
When AllowRowIDUpdate is enabled (n = 1), RowID values are user-modifiable. Modifying Ro wID values can have serious consequences and should only be done in very specific cases and with e xtreme caution. Set to 1 only if you are doing your own filing in a BEFORE trigger and using the %SkipFiling flag. Otherwise, use the def

ault of 0.

You can change AllowRowIDUpdate with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

- RowID Field

AutoParallel

AutoParallel

Allow parallel processing instance-wide.

Synopsis

[SQL] AutoParallel=n

n is either 1 or 0. The default value is 1.

Description
When AutoParallel is enabled (n = 1), InterSystems SQL queries can use parallel processing to run more efficiently . In sharded environments, this means all queries are executed using parallel processing. In non-sharded environments, InterSystems SQL determines per-query whether to use parallel processing, based on the value of AutoParallelThreshold.

When this parameter is not enabled, all queries run in a single process.

If AdaptiveMode is enabled (set to 1) and AutoParallel is disabled, then Adaptive Mode overrides the AutoParallel setting and enables parallel processing.

On the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL), select Execute queries in a single process to disable parallel processing.

To set the desired value for AutoParallel from the Terminal, use the SetOption(“AutoParallel”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change AutoParallel with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[SQL]

AutoParallelThreshold

Set the threshold for parallel processing.

Synopsis

[SQL] AutoParallelThreshold=n

n is any nonnegative integer. The default value is 3200.

Description
The higher n is, the lower the chance that an InterSystems SQL query executes using parallel processing. The value n corresponds roughly to the minimal number of tuples needed in the visited map for parallel processing to occur.

When AutoParallel is disabled, AutoParallelThreshold has no effect.

To set the desired value for AutoParallelThreshold from the Terminal, use the SetOption(“AutoParallelThreshold”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change AutoParallelThreshold with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

AutoStatsForEfficientSamplingTableOnly

AutoStatsForEfficientSamplingTableOnly

Determine whether to collect statistics only from tables that support efficient sampling.

Synopsis

[SQL] AutoStatsForEfficientSamplingTableOnly=n

n is either 0 or 1. The default value is 1.

Description
When AutoStatsForEfficientSamplingTableOnly is enabled, the automatic collected statistics utility collects statistics only for tables that support efficient sampling.

On the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL), select Disable automatic statistics collection for tables supporting efficient sampling only. to ignore the requirement that the automatic collected statistics utility tables support efficient sampling (that is, change the v alue to 0).

You can also change AutoStatsForEfficientSamplingTableOnly by editing the CPF in a text editor (as described in Editing the Active CPF).

[SQL]

AutoStatsForFixedStatsTable

Determine whether to gather collected statistics for tables that already have a set of fix ed statistics.

Synopsis

[SQL] AutoStatsForFixedStatsTable=n

n is either 0 or 1. The default value is 1.

Description
When AutoStatsForFixedStatsTable is enabled, the automatic collected statistics utility gathers and stores collected statistics for tables that have predefined fix statistics on tables that have predefined fix

ed statistics. When it is disabled, the utility neither gathers nor stores such ed statistics.

On the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL), select Disable automatic statistics collection for tables with fixed statistics. to prevent the automatic collected statistics utility from gather statistics for tables that already have a set of fix ed statistics.

You can also change AutoStatsForFixedStatsTable by editing the CPF in a text editor (as described in Editing the
Active CPF).

AutoStatsForRemoteGlobalTable

Determine whether the collected statistics utility should gather statistics for tables whose data is stored in globals that map to remote databases.

AutoStatsForRemoteGlobalTable

Synopsis

[SQL] AutoStatsForRemoteGlobalTable=n

n is either 0 or 1. The default value is 0.

Description
When AutoStatsForRemoteGlobalTable is enabled, the automatic collected statistics utility does not gather statistics for tables whose data is stored in globals that map to remote databases.

On the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL), select Enable automatic statistics collection for tables whose default data global maps to remote databases. to .

You can also change AutoStatsForRemoteGlobalTable by editing the CPF in a text editor (as described in Editing the Active CPF).

[SQL]

BiasQueriesAsOutlier

Set query optimization to biased toward outliers.

Synopsis

[SQL] BiasQueriesAsOutlier=n

n is either 1 or 0. The default value is 1.

Description
When BiasQueriesAsOutlier is enabled (n = 1), InterSystems SQL optimizes for queries that primarily return outlier values.

BiasQueriesAsOutlier and RTPC cannot both be active at the same time. If both BiasQueriesAsOutlier and RTPC are set to 1, RTPC is activated and the BiasQueriesAsOutlier setting is ignored. When RTPC is set, InterSystems SQL determines whether to use outlier optimization on a per-query basis.

If AdaptiveMode is enabled (set to 1) and BiasQueriesAsOutlier is active and enabled, then Adaptive Mode overrides the BiasQueriesAsOutlier setting and disables outlier bias.

Note:

Adaptive Mode replaces the earlier BiasQueriesAsOutlier mechanism. Please contact the WRC is you were previously using this setting and need assistance.

You can change BiasQueriesAsOutlier with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

ClientMaxIdleTime

ClientMaxIdleTime

Synopsis

[SQL] ClientMaxIdleTime=n

n is the number of seconds that a the system allows for an idle connection between the client and the server before forcing a disconnection. The default value is 0.

Description
When ClientMaxIdleTime is not 0, InterSystems SQL will wait the specified number of seconds before closing an idle connection between a client and a server. This timeout applies to all SQL clients that have been sitting idle, including JDBC, ODBC, and ADO.NET.

On the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL), enter a value for the Client maximum idle time (seconds) setting.

You can change ClientMaxIdleTime by using SetOption() or by editing the CPF in a text editor (as described in Editing the Active CPF). In addition, you can change the parameter on the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL) by entering a value for the Client maximum idle time (seconds) setting.

[SQL]

Comment

Retain embedded SQL statements as comments in source code.

Synopsis

[SQL] Comment=n

n is either 1 or 0. The default value is 1.

Description
When Comment is enabled (n = 1), embedded SQL statements are retained as comments in the source code (.INT) version of the routine.

To set the desired value for Comment from the Terminal, use the SetOption(“RetainSQL”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change Comment with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

DDLDefineBitmapExtent

DDLDefineBitmapExtent

Specify whether a table created by a DDL statement defines a bitmap e xtent index.

Synopsis

[SQL] DDLDefineBitmapExtent=n

n is either 1 or 0. The default value is 1.

Description
When DDLDefineBitmapExtent is enabled (n = 1), a table created by a DDL CREATE TABLE statement defines a bitmap extent index. The index improves the performance of COUNT(*), a function that returns the number of rows in the table.

To set the desired value for DDLDefineBitmapExtent from the Terminal, use the SetOption(“DDLDefineBitmapExtent”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change DDLDefineBitmapExtent with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[SQL]

DDLFinal

Specify whether a class created by a DDL statement is final.

Synopsis

[SQL] DDLFinal=n

n is either 1 or 0. The default value is 1.

Description
When DDLFinal is enabled (n = 1), a class created by a DDL CREATE TABLE statement is final subclasses.

, meaning it cannot have

To set the desired value for DDLFinal from the Terminal, use the SetOption(“DDLFinal”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change DDLFinal with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

DDLNo201

DDLNo201

Suppress error upon CREATE of a previously existing table.

Synopsis

[SQL] No201=n

n is either 1 or 0. The default value is 0.

Description
When DDLNo201 is enabled (n = 1), when an attempt is made to CREATE a previously existing table or view, InterSystems IRIS® data platform suppresses the SQLCODE -201 error. When this parameter is not enabled, InterSystems IRIS returns the error.

For further details, refer to the CREATE TABLE and CREATE VIEW commands.

On the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL), select Ignore redundant DDL statements to suppress redundant SQLCODE errors.

You can also change DDLNo201 with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[SQL]

DDLNo30

Suppress error upon DROP of a nonexistent table.

Synopsis

[SQL] DDLNo30=n

n is either 1 or 0. The default value is 0.

Description
When DDLNo30 is enabled (n = 1), when an attempt is made to DROP a nonexistent table, InterSystems IRIS® data platform suppresses the SQLCODE -30 error. When this parameter is not enabled, InterSystems IRIS returns the error.

For further details, refer to the DROP TABLE and DROP VIEW commands.

On the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL), select Ignore redundant DDL statements to suppress redundant SQLCODE errors.

You can also change DDLNo30 with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

DDLNo307

DDLNo307

Suppress error upon CREATE of a primary key constraint when one exists.

Synopsis

[SQL] DDLNo307=n

n is either 1 or 0. The default value is 0.

Description
When DDLNo307 is enabled (n = 1), when an attempt is made to CREATE a primary key constraint to a table through DDL, and a primary key constraint already exists for that table, InterSystems IRIS® data platform suppresses the SQLCODE -307 error.

For further details, refer to the CREATE TABLE and ALTER TABLE commands.

On the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL), select Ignore redundant DDL statements to suppress redundant SQLCODE errors.

You can also change DDLNo307 with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[SQL]

DDLNo311

Suppress error upon ADD a foreign key, when a key of that name already exists.

Synopsis

[SQL] DDLNo311=n

n is either 1 or 0. The default value is 0.

Description
When DDLNo311 is enabled (n = 1), when an attempt is made to ADD a foreign key, even if a key of that name already exists, InterSystems IRIS® data platform suppresses the SQLCODE -311 error.

For further details, refer to the ALTER TABLE command.

On the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL), select Ignore redundant DDL statements to suppress redundant SQLCODE errors.

You can also change DDLNo311 with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

DDLNo315

DDLNo315

Suppress error upon DROP of a nonexistent constraint.

Synopsis

[SQL] DDLNo315=n

n is either 1 or 0. The default value is 0.

Description
When DDLNo315 is enabled (n = 1), when an attempt is made to DROP a nonexistent constraint, InterSystems IRIS® data platform suppresses the SQLCODE -315 error.

For further details, refer to the ALTER TABLE command.

On the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL), select Ignore redundant DDL statements to suppress redundant SQLCODE errors.

You can also change DDLNo315 with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[SQL]

DDLNo324

Suppress error upon CREATE of a previously existing index.

Synopsis

[SQL] DDLNo324=n

n is either 1 or 0. The default value is 0.

Description
When DDLNo324 is enabled (n = 1), when an attempt is made to CREATE a previously existing index, InterSystems IRIS® data platform suppresses the SQLCODE -324 error.

For further details, refer to the CREATE INDEX command.

On the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL), select Ignore redundant DDL statements to suppress redundant SQLCODE errors.

You can also change DDLNo324 with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

DDLNo333

DDLNo333

Suppress error upon DROP of a nonexistent index.

Synopsis

[SQL] DDLNo333=n

n is either 1 or 0. The default value is 0.

Description
When DDLNo333 is enabled (n = 1), when an attempt is made to DROP a nonexistent index, InterSystems IRIS® data platform suppresses the SQLCODE -333 error.

For further details, refer to the DROP INDEX command.

On the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL), select Ignore redundant DDL statements to suppress redundant SQLCODE errors.

You can also change DDLNo333 with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[SQL]

DDLSQLOnlyCompile

Enable an SQL-only compile.

Synopsis

[SQL] DDLSQLOnlyCompile=n

n is either 1 or 0. The default value is 0.

Description
When DDLSQLOnlyCompile is enabled (n = 1), any class compilation performed as a result of executing a DDL statement compiles the class with the q (sqlonly) flag. When this parameter is not enabled, the q flag is not used.

You can change DDLSQLOnlyCompile with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

DDLUseExtentSet

DDLUseExtentSet

Allow hashed names for globals that store index data of tables created by a DDL statement.

Synopsis

[SQL] DDLUseExtentSet=n

n is either 1 or 0. The default value is 1.

Description
When DDLUseExtentSet is enabled (n = 1), a table created by a DDL CREATE TABLE statement stores its index data in globals that use hashed names. The hashed names typically allow for better performance when running queries against the table, but are less comprehensible to the user. When this parameter is not enabled, the index data is stored in globals named after the class.

To set the desired value for DDLUseExtentSet from the Terminal, use the SetOption(“DDLUseExtentSet”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change DDLUseExtentSet with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[SQL]

DDLUseSequence

Specify the function a table created by a DDL statement uses for ID assignment.

Synopsis

[SQL] DDLUseSequence=n

n is either 1 or 0. The default value is 1.

Description
When DDLUseSequence is enabled (n = 1), a table created by DDL CREATE TABLE uses $SEQUENCE for ID
assignment. When this parameter is not enabled, the table uses $INCREMENT.

$SEQUENCE is the default function, and is better suited for ID assignment. For a comparison of the two functions, see
$INCREMENT or $SEQUENCE.

To set the desired value for DDLUseSequence from the Terminal, use the SetOption(“DDLUseSequence”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change DDLUseSequence with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

DefaultSchema

DefaultSchema

Set the default SQL schema name.

Synopsis

[SQL] DefaultSchema=n

n is a string with a maximum length of 128 characters. The default string is SQLUser.

Description
DefaultSchema defines the def ault SQL schema name. The default schema name comes into play when an unqualified table name is encountered in an SQL statement and there is no #import statement specified. This setting has nothing to
do with the mappings between SQL schema names and the class package name; it only specifies the def ault schema name.

If you specify _CURRENT_USER as the default schema name, the default schema name becomes the username of the currently logged-in process or, if the process has not logged in, SQLUser becomes the default schema name.

If you specify _CURRENT_USER/name as the default schema name, where name is any string of your choice, then the default schema name becomes the username of the currently logged-in process or, if the process has not logged in, name is used as the default schema name. For example, _CURRENT_USER/HMO uses HMO as the default schema name if the process has not logged in.

For further details, refer to the CREATE TABLE and CREATE VIEW commands.

On the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL), for the Default Schema setting, enter a string of characters.

You can also change DefaultSchema with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[SQL]

DelimitedIds

Enable interpreting double-quoted strings as delimited identifiers.

Synopsis

[SQL] DelimitedIds=n

n is either 1 or 0. The default value is 1.

Description
When DelimitedIds is enabled (n = 1), a double-quoted string ("My String") is considered a delimited identifier within an SQL statement. When this parameter is not enabled, a double-quoted string ("My String") is considered a string constant or literal string.

For further details, refer to the SET OPTION command. Also see SQL Identifiers .

To set the desired value for DelimitedIds from the Terminal, use the SetOption(“DelimitedIdentifiers”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change DelimitedIds with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

DropDelete

DropDelete

Specify whether DROP TABLE deletes the table’s data in addition to the table.

Synopsis

[SQL] DropDelete=n

n is either 1 or 0. The default value is 1.

Description
When DropDelete is enabled (n = 1), a DROP TABLE statement deletes the table and the table's data. When this parameter is not enabled, a DROP TABLE statement deletes the table, but does not delete the data.

For further details, refer to the DROP TABLE command.

To set the desired value for DropDelete from the Terminal, use the SetOption(“DDLDropTabDelData”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change DropDelete with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[SQL]

ECPSync

Ensure that the server and client cache are in sync.

Synopsis

[SQL] ECPSync=n

n is either 1 or 0. The default value is 0.

Description
When ECPSync is enabled (n = 1), each time a SELECT statement is executed InterSystems IRIS® data platform forces all pending Enterprise Cache Protocol (ECP) requests to the database server. On completion, this guarantees that the client cache is in sync.

ECP is a distributed data caching architecture that manages the distribution of data and locks among a heterogeneous network of server systems. For further details, see Queries and ECP.

To set the desired value for ECPSync from the InterSystems Terminal, use the SetOption(“ECPSync”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change ECPSync with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

ExtrinsicFunctions

ExtrinsicFunctions

Enable extrinsic functions in SQL statements.

Synopsis

[SQL] ExtrinsicFunctions=n

n is either 1 or 0. The default value is 0.

Description
When ExtrinsicFunctions is enabled (n = 1), extrinsic functions can be used in SQL statements through ODBC, JDBC, and Dynamic Query.

For further details, refer to the SELECT command.

To set the desired value for ExtrinsicFunctions from the Terminal, use the SetOption(“AllowExtrinsicFunctions”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change ExtrinsicFunctions with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[SQL]

FastDistinct

Allow SQL DISTINCT optimization.

Synopsis

[SQL] FastDistinct=n

n is either 1 or 0. The default value is 1.

Description
When FastDistinct is enabled (n = 1), SQL queries involving DISTINCT, GROUP BY, or UNION clauses run more efficiently by making better use of inde xes (if indexes are available).

CAUTION:

The values returned by such queries are collated in the same way they are stored within the index. This means the results of such queries may be all uppercase. This may have an effect on case-sensitive applications.

For further details, refer to the GROUP BY clause, the DISTINCT clause, and the UNION clause of the SELECT statement.

FastDistinct is enabled by default. On the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL), select GROUP BY and DISTINCT queries must produce original values to disable
FastDistinct.

To set the desired value for FastDistinct from the Terminal, use the SetOption(“FastDistinct”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change FastDistinct with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

IdKey

IdKey

Set primary key constraint behavior.

Synopsis

[SQL] IdKey=n

n is either 1 or 0. The default value is 1.

Description
When IdKey is enabled (n = 1), when a Primary Key constraint is specified through DDL it does not also become the IDKey index in the class definition.

When this parameter is not enabled, a Primary Key constraint specified through DDL also becomes the IDK ey index in the class definition. This option generally gives better performance, but means that the Primary Key fields cannot be updated.

For further details, refer to the SET OPTION, CREATE TABLE, and ALTER TABLE commands.

On the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL), select Define primary key as ID key for tables created via DDL to disable IdKey.

To set the desired value for IdKey from the Terminal, use the SetOption(“DDLPKeyNotIDKey”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change IdKey with the Config.SQL class (as described in the class reference) or by editing the CPF in a te xt editor (as described in Editing the Active CPF).

[SQL]

IdTrxFrom

Define the “From” list of characters for Identifier Translation.

Synopsis

[SQL] IdTrxFrom=n

n is a string with a maximum length of 256 characters. The default string is ~ `@#$%^&*()_+-=[]\{}|;':"",./<>?"..

Description
IdTrxFrom is a string of characters that provides the “From” list for DDL Identifier Translation mappings. These mappings filter/modify v alid SQL identifier characters when translating SQL identifiers into Objects identifiers. When converting an SQL identifier to an Objects identifier at DDL runtime, the characters in the in the “To” string.

“From” string are converted to the characters

For further details, see Identifiers . Also see the parameter IdTrxTo.

To set the desired value for IdTrxFrom from the Terminal, use the SetDDLIdentifierT ranslations() method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change IdTrxFrom with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

IdTrxTo

IdTrxTo

Define the “To” list of characters for Identifier Translation.

Synopsis

[SQL] IdTrxTo=n

n is a string with a maximum length of 256 characters. The default is an empty string.

Description
IdTrxTo is a string of characters that provides the “To” list for the DDL Identifier Translation mappings.

For further details, see Identifiers . Also see IdTrxFrom.

To set the desired value for IdTrxTo from the Terminal, use the SetDDLIdentifierT ranslations() method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change IdTrxTo with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[SQL]

LockThreshold

Set the SQL table-level lock threshold.

Synopsis

[SQL] LockThreshold=n

n is any nonnegative integer. The default value is 1000.

Description
The LockThreshold parameter is the automatic lock escalation threshold. This is the number of inserts, updates, or deletes for a single table within a single transaction that will trigger a table-level lock when reached.

Consider this example: a process starts a transaction that inserts 2000 rows, where LockThreshold is set to 1000. After the 1001st row is inserted, the process attempts to acquire a table-level lock rather than continue to lock individual rows. This reduces the total number of locks to prevent the lock table from becoming too full.

Automatic lock escalation is intended to prevent overflo w of the lock table. For further details, see Modify Transaction
Lock Threshold.

On the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL), in the Lock escalation threshold field, enter a number .

To set the desired value for LockThreshold from the Terminal, use the SetOption(“LockThreshold”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change LockThreshold with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

If the LockThreshold parameter is changed, any new process started after changing it will automatically have the new setting, without the need to restart InterSystems IRIS.

- The gmheap and locksiz parameters (to increase the size of the lock table).

- The Lock Table

- Modify Transaction Lock Threshold in InterSystems SQL Reference.

LockTimeout

LockTimeout

Set the SQL lock timeout.

Synopsis

[SQL] LockTimeout=n

n is an integer in the range 0—32,767. The default value is 10.

Description
LockTimeout is the lock timeout (in seconds) for InterSystems IRIS® data platform locks made during execution of SQL statements. The maximum value is 32,767 seconds, or 9 hours.

For further details, refer to the SET OPTION command.

On the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL), for the Lock Timeout (seconds) setting, enter a number.

To set the desired value for LockTimeout from the InterSystems Terminal, use the SetOption(“LockTimeout”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change LockTimeout with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[SQL]

ODBCVarcharMaxlen

Set the MaxLen for ODBC fields of type VarChar.

Synopsis

[SQL] ODBCVarcharMaxlen=n

n is any nonnegative integer. The maximum value is the maximum string length. The default value is 4096.

Description
ODBCVarcharMaxlen is the MaxLen (maximum length) that InterSystems IRIS® data platform will report to ODBC for fields with the data type VarChar.

You can change ODBCVarcharMaxlen with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

ParameterSampling

ParameterSampling

Specify whether parameter sampling is enabled or not.

Synopsis

[SQL] ParameterSampling=n

n is either 1 or 0. The default is 0.

Description
When ParameterSampling is enabled (n = 1), InterSystems SQL will save the complete set of query parameters when statements are prepared. This data is projected to INFORMATION_SCHEMA.STATEMENT_PARAMETER_STATS, which includes information on the efficienc y of the query. Enabling parameter sampling can be useful for evaluating the efficac y of a schema, especially when testing a new schema.

On the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL), select Turn on parameter sampling to sample the parameter value for query execution to enable ParameterSampling.

To set the desired value for ParameterSampling from the InterSystems Terminal, use the SetOption(“ParameterSampling”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change ParameterSampling with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[SQL]

QueryProcedures

Specify whether all class queries project as SQL Stored Procedures.

Synopsis

[SQL] QueryProcedures=n

n is either 1 or 0. The default value is 0.

Description
When QueryProcedures is enabled (n = 1), all SQL class queries project as SQL Stored Procedures, regardless of the query’s SqlProc value. When this parameter is not enabled, only class queries defined with SqlProc=1 project as Stored
Procedures.

When changing this setting, you must recompile the classes with the class queries in order for this change to have an affect. Modifying this setting in the CPF does not require a n instance restart to make it active.

To set the desired value for QueryProcedures from the Terminal, use the SetOption(“QueryProcedures”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change QueryProcedures with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

RTPC

RTPC

Enable Runtime Plan Choice (RTPC) query optimization.

Synopsis

[SQL] RTPC=n

n is either 1 or 0. The default value is 1.

Description
When RTPC is enabled (n = 1), InterSystems SQL performs extensive optimization of the query based on query input values.
For example, RTPC does the following:

- Scans for outlier values and optimizes queries based on outlier information.

- Efficiently estimates the selecti vity of range conditions based on more detailed table statistics.

- Efficiently estimates the selecti vity of list conditions.

- Evaluates explicit and implicit truth value conditions.

- Evaluates whether certain resource-expensive functions, such as VECTOR_COSINE, are called on identical expressions in the SELECT clause and the ORDER BY clause of the same query to reduce the number of times the function is executed.

For more information about RTPC query optimization, see Using Runtime Plan Choice.

RTPC and BiasQueriesAsOutlier cannot both be active at the same time. If both RTPC and BiasQueriesAsOutlier are set to 1, RTPC is activated and the BiasQueriesAsOutlier setting is ignored. When RTPC is set, InterSystems SQL determines whether to use outlier optimization on a per-query basis.

If AdaptiveMode is enabled (set to 1) and RTPC is disabled, then Adaptive Mode overrides the RTPC setting and enables Runtime Plan Choice.

To set the desired value for RTPC from the Terminal, use the SetOption(“RTPC”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change RTPC with the Config.SQL class (as described in the class reference) or by editing the CPF in a te xt editor (as described in Editing the Active CPF).

[SQL]

ReferentialChecks

Enable foreign key constraint validation.

Synopsis

[SQL] ReferentialChecks=n

n is either 1 or 0. The default value is 1.

Description
When ReferentialChecks is enabled (n = 1), InterSystems IRIS® data platform validates the foreign key constraint for INSERT, UPDATE, DELETE, and TRUNCATE TABLE operations. When this parameter is not enabled, InterSystems IRIS bypasses validation of foreign key constraints.

For further details, refer to the DELETE, INSERT, TRUNCATE TABLE, and UPDATE commands in the InterSystems
SQL Reference.

To set the desired value for ReferentialChecks from the InterSystems Terminal, use the SetOption(“FilerRefIntegrity”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change ReferentialChecks with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

SaveMAC

SaveMAC

Save the source code for cached query routines.

Synopsis

[SQL] SaveMAC=n

n is either 1 or 0. The default value is 0.

Description
When SaveMac is enabled (n = 1), the source code (.MAC and .INT) for cached query routines created through Dynamic SQL is saved.

On the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL), select Retain cached query source to enable SaveMac.

To set the desired value for SaveMac from the Terminal, use the SetOption(“CachedQuerySaveSource”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change SaveMac with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[SQL]

TCPKeepAlive

Set the number of seconds between keep-alive messages.

Synopsis

[SQL] TCPKeepAlive=n

n is an integer in the range 30—432,000. The default value is 300.

Description
TCPKeepAlive is the number of seconds between keep-alive messages. The setting applies only to InterSystems IRIS® data platform running on Windows and Linux. The default is 300 seconds (5 minutes), and the maximum value is 432,000 (5 days). If the value is 0, the instance uses the operating system default.

For further details, see TCP Client/Server Communication.

On the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL), enter a value for the TCP keepalive for client connections (seconds) setting.

To set the desired value for TCPKeepAlive from the InterSystems Terminal, use the SetOption(“TCPKeepAlive”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change TCPKeepAlive with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

For a TCP device, you can also specify this interval with the OPEN or USE commands, as the eighth parameter (for example: O tcp:("SERVER":port:mode:::::keepalive). or the keyword /KEEPALIVE=n. The OPEN command
parameters for a TCP device are:
hostname{:port{:mode{:terminators{:ibfsz{:obfsz{:queuesize{:keepalivetime}}}}}}}. For
details about the OPEN and USE commands and arguments, including examples, see the ObjectScript Reference.

TODATEDefaultFormat

TODATEDefaultFormat

Set the default date format for the SQL TO_DATE() function.

Synopsis

[SQL] TODATEDefaultFormat=n

n is any string in a format appropriate for the TO_DATE() function. The default string is DD MON YYYY.

Description
The value n provides the format string that the SQL TO_DATE() function uses when TO_DATE() is called without a format specified.

Examples

The following is an example:

TODATEDefaultFormat=DD MON YYYY

And the following another example:

TODATEDefaultFormat=YYYY DD MM

For more examples and an in-depth discussion about valid date strings, see the format argument description of the TO_DATE reference page.

On the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL), in the TO_DATE default format field, enter a v alid format string.

To set the desired value for TODATEDefaultFormat from the Terminal, use the SetOption(“ToDateDefaultFormat”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change TODATEDefaultFormat with the Config.SQL class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[SQL]

TimePrecision

Set the default time precision for SQL scalar time functions.

Synopsis

[SQL] TimePrecision=n

n is an integer in the range 0—9. The default value is 0.

Description
TimePrecision defines the def ault number of decimal places in the value returned by the SQL scalar functions GETDATE(), CURRENT_TIME, CURRENT_TIMESTAMP, GETUTCDATE, and UNIX_TIMESTAMP. A value
returned by these function has n decimal places for fractional seconds. The actual precision possible is platform dependant;
precision digits in excess of the precision available on your system are returned as zeros.

On the SQL page of the Management Portal (System Administration > Configuration > SQL and Object Settings > SQL), for the Default time precision for GETDATE(), CURRENT_TIME, and CURRENT_TIMESTAMP setting, select a number of decimal places.

To set the desired value for TimePrecision from the Terminal, use the SetOption(“DefaultTimePrecision”) method of the %SYSTEM.SQL.Util class. See the class reference for details.

You can also change TimePrecision with the Config.SQL class (as described in the class reference), by editing the CPF in a text editor (as described in Editing the Active CPF), or by using the SET OPTION command.

[SqlSysDatatypes]

This topic describes the parameters found in the [SqlSysDatatypes] section of the CPF.

[SqlSysDatatypes]

System Datatypes

Map SQL datatypes to their InterSystems IRIS® data platform equivalents.

Synopsis

[SqlSysDatatypes] x=a

x is the name of an SQL datatype. a is the InterSystems IRIS equivalent.

Description
The [SqlSysDatatypes] section contains system-defined datatype descriptions. Each description maps an SQL datatype
to its InterSystems IRIS equivalent in the format x=a, as follows:

- Each keyword x is the name of the SQL datatype, plus any allowed arguments.

- The value a is the InterSystems IRIS equivalent, including any constraints on the arguments.

Example

The following excerpt of a configuration parameter file sho ws the [SqlSysDatatypes] which begin with the letter B. For a table of all System-Defined DDL Datatype mappings, see Table of DDL Data Types.

[SqlSysDatatypes]
BIGINT=%Library.BigInt
BIGINT(%1)=%Library.BigInt
BINARY=%Library.Binary(MAXLEN=1)
BINARY VARYING=%Library.Binary(MAXLEN=1)
BINARY VARYING(%1)=%Library.Binary(MAXLEN=%1)
BINARY(%1)=%Library.Binary(MAXLEN=%1)
BIT=%Library.Boolean

On the System-defined DDL Mappings page of the Management Portal (System Administration > Configuration > SQL and Object Settings > System DDL Mappings), select Edit to modify a datatype definition.

Instead of using the Management Portal, you can modify datatype definitions by editing the CPF in a te xt editor (as described in Editing the Active CPF).

- User Datatypes

- Data Types

[SqlUserDatatypes]

This topic describes the parameters found in the [SqlUserDatatypes] section of the CPF.

[SqlUserDatatypes]

User Datatypes

Map SQL datatypes to their InterSystems IRIS® data platform equivalents.

Synopsis

[SqlUserDatatypes] x=a

x is the name of an SQL datatype. a is the InterSystems IRIS equivalent.

Description
The [SqlUserDatatypes] section contains a user-defined datatype descriptions. Each description maps an SQL datatype
to its InterSystems IRIS equivalent in the format x=a, as follows:

- Each keyword x is the name of the SQL datatype, plus any allowed arguments.

- The value a is the InterSystems IRIS equivalent, including any constraints on the arguments.

Example

With the following line in the CPF, when MYVARCHAR(10) is seen in a statement, the property is created with type
%Library.String(MAXLEN=10,TRUNCATE=0).

[SqlUserDatatypes]
MYVARCHAR(%1)=%Library.String(MAXLEN=%1,TRUNCATE=0)

On the User-defined DDL Mappings page of the Management Portal (System Administration > Configuration > SQL and Object Settings > User DDL Mappings), click Create New User-defined DDL Mapping to add a new datatype mapping, or click Edit to modify an existing one.

Instead of using the Management Portal, you can modify datatype definitions in the Config.SqlUserDatatypes class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

- System Datatypes

- Data Types

[Startup]

This topic describes the Startup Settings parameters found in the [Startup] section of the CPF. Many of these, including the mirror-related and sharded cluster-related parameters, are in deployment with configuration mer ge.

[Startup]

CallinHalt

Allow custom routines during callin close.

Synopsis

[Startup] CallinHalt=n

n is either 1 (true) or 0 (false). The default value is 1.

Description
When CallinHalt is enabled (n = 1), InterSystems IRIS® data platform executes the CALLIN^%ZSTOP routine entry each time an external program ends a CALLIN. When this parameter is not enabled, the routine is not executed.

For more information about CALLIN, see Using The Callin API.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the CallinHalt row, select Edit. Select CallinHalt to enable this setting.

Instead of using the Management Portal, you can change CallinHalt in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

ProcessStart and ProcessHalt correspond to foreground processes. These are processes that are started via a terminal session or are specifically set to run in the fore ground.

JobStart and JobHalt correspond to background processes. This includes any processes that are started via the JOB command, plus any background server processes including ODBC or any of the object bindings.

SystemStart and SystemHalt correspond to InterSystems IRIS instance startup or shutdown.

CallinStart and CallinHalt correspond to external programs performing a CALLIN.

For more information about all these parameters, see Customizing Start and Stop Behavior with ^%ZSTART and ^%ZSTOP.

CallinStart

CallinStart

Allow custom routines during callin initialization.

Synopsis

[Startup] CallinStart=n

n is either 1 (true) or 0 (false). The default value is 1.

Description
When CallinStart is enabled (n = 1), InterSystems IRIS® data platform executes the CALLIN^%ZSTART routine entry each time an external program begins a CALLIN. When this parameter is not enabled, the routine is not executed.

For more information about CALLIN, see Using The Callin API.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the CallinStart row, select Edit. Select CallinStart to enable this setting.

Instead of using the Management Portal, you can change CallinStart in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

ProcessStart and ProcessHalt correspond to foreground processes. These are processes that are started via a terminal session or are specifically set to run in the fore ground.

JobStart and JobHalt correspond to background processes. This includes any processes that are started via the JOB command, plus any background server processes including ODBC or any of the object bindings.

SystemStart and SystemHalt correspond to InterSystems IRIS instance startup or shutdown.

CallinStart and CallinHalt correspond to external programs performing a CALLIN.

For more information about all these parameters, see Customizing Start and Stop Behavior with ^%ZSTART and ^%ZSTOP.

[Startup]

CliSysName

Set the node name for the local system.

Synopsis

[Startup] CliSysName=n

n is a string with a maximum length of 64 characters. The default is an empty string.

Description
CliSysName is the node name for this computer, and is used as:

- The node name to be sent to the ECP network server, so that the server can identify the client.

- The node name for a unique $JOB value. This is useful when using $JOB to index globals accessed by more than one
networked system.

- The node name returned by certain forms of the $SYSTEM function, concatenated with the InterSystems IRIS® data
platform instance name, as nodename:instancename. This concatenated string is recorded in Audit files.

If no name is provided, InterSystems IRIS reads the computer settings and uses the computer “host name” as the client node name.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the CliSysName row, select Edit. Enter the desired node name.

Instead of using the Management Portal, you can change CliSysName in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

DBSizesAllowed

DBSizesAllowed

Specify possible database block sizes when creating a database.

Synopsis

[Startup] DBSizesAllowed=n[n,n...]

n can be 8192, 16384, 32768, or 65536. The default value is 8192.

Description
DBSizesAllowed lists the database block sizes (in bytes) that you can select when creating a database.

For more information about creating and managing databases, see Configuring Databases .

Examples

DBSizesAllowed=8192,16384

DBSizesAllowed=8192,65536

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the DBSizesAllowed row, select Edit. Select each desired database block size. You cannot clear 8192.

Important: When you enable an additional database block size, you must allocate memory for that block size using the globals parameter. This allows InterSystems IRIS® data platform to create the needed pool of global buffers for that size.

Instead of using the Management Portal, you can change DBSizesAllowed in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF file ).

- globals parameter in the [Config] section of this reference

- Configuring Databases

- Allocating Memory to the Database and Routine Caches

- Large Block Size Considerations

[Startup]

DefaultPort

Set the port number for the InterSystems IRIS® data platform superserver.

Synopsis

[Startup] DefaultPort=n

n is a valid port number. The default is 1972.

Description
DefaultPort is the port number for the InterSystems IRIS superserver. The superserver listens on a specified port (1972 by default) for incoming connections to InterSystems IRIS and dispatches them to the appropriate subsystem.

A standard InterSystems IRIS installation sets the superserver port number to 1972. If that port is in use by another Inter- Systems IRIS instance on the same system, then InterSystems IRIS sets this value to 51773 or the next available subsequent number.

On the Memory and Startup page of the Management Portal (System Administration > Configuration > System Configuration > Memory and Startup), enter a number in the Superserver Port Number field.

Instead of using the Management Portal, you can change DefaultPort in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

DefaultPortBindAddress

DefaultPortBindAddress

Set the IP address for the InterSystems IRIS® data platform superserver to bind to.

Synopsis

[Startup] DefaultPortBindAddress=nnn.nnn.nn.nn

nnn.nnn.nn.nn is a valid IP address. By default, none is specified.

Description
DefaultPortBindAddress is the IP addresses on the host system that the superserver should bind to. The superserver is the process that accepts client connections for ODBC, JDBC, and other connection technologies. Requests to the superserver port on other IP addresses on the host are not accepted. This makes it possible to limit connections to the superserver to a single address on a multihomed host.

If this property is not set, the superserver accepts requests on all IP addresses on the host. The default is to accept on all addresses.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the DefaultPortBindAddress row, select Edit. Enter an IP address.

Instead of using the Management Portal, you can change DefaultPortBindAddress in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[Startup]

EnableSharding

Enable the sharding service for this instance.

Synopsis

[Startup] EnableSharding=n

n is either 1 (true) or 0 (false).

Description
When this parameter is enabled (n = 1), InterSystems IRIS® data platform enables the Sharding service (%Service_Sharding) during instance startup. When EnableSharding is disabled (n = 0), %Service_Sharding is unchanged during instance startup. This means EnableSharding can only be used to enable the Sharding service, not to disable it.

EnableSharding is not in the default CPF, but can be included as a parameter in a configuration mer ge file . To directly enable %Service_Sharding without the EnableSharding parameter, use the Services page of the Management Portal (System Administration > Security > Services).

EnableSharding is not in the default CPF. You can manually add this parameter to the [Startup] section of the CPF using a text editor (as described in Editing the Active CPF).

After adding EnableSharding and restarting InterSystems IRIS, you can change the parameter using a text editor, the Config.Startup class (as described in the class reference), or the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup).

- Services.

EnableVSSBackup

EnableVSSBackup

Allow Volume Shadow Copy Service (VSS) backup. Windows systems only.

Synopsis

[Startup] EnableVSSBackup=n

n is either 1 (true) or 0 (false). The default value is 1.

Description
When EnableVSSBackup is enabled (n = 1), InterSystems IRIS® data platform supports VSS on Windows. VSS is only available on Windows. On other platforms, InterSystems IRIS ignores the EnableVSSBackup parameter.

See Backup Strategies for information about creating a backup using VSS or other methods.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the EnableVSSBackup row, select Edit. Select EnableVSSBackup to enable this setting.

Instead of using the Management Portal, you can change EnableVSSBackup in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[Startup]

EnsembleAutoStart

Allow productions to auto-start when InterSystems IRIS® data platform starts.

Synopsis

[Startup] EnsembleAutoStart=n

n is either 1 (true) or 0 (false). The default value is 1.

Description
When EnsembleAutoStart is enabled, the production you set to auto-start in each interoperability-enabled namespace starts when you start InterSystems IRIS. To facilitate debugging situations involving troubled productions, you can disable this setting to prevent a production from starting.

For details on how this setting works with production settings, see the description of the Auto-Start Production field in Starting and Stopping Productions.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the EnsembleAutoStart row, select Edit. Select EnsembleAutoStart to enable this setting.

Instead of using the Management Portal, you can change EnsembleAutoStart in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

ErrorPurge

ErrorPurge

Set the number of days to keep error globals.

Synopsis

[Startup] ErrorPurge=n

n is an integer in the range 1—1000. The default value is 30.

Description
ErrorPurge is the number of days to keep the error globals for the ^%ETN error handler. Errors older than this are deleted on the next InterSystems IRIS® data platform restart.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the ErrorPurge row, select Edit. Enter a number of days.

Instead of using the Management Portal, you can change ErrorPurge in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

If you edit this setting, the change is applied the next time you restart InterSystems IRIS.

[Startup]

FIPSMode

Enable FIPS 140–2 compliant library for database encryption on Red Hat Linux.

Synopsis

[Startup] FIPSMode=n

n is either 1 (true) or 0 (false). The default value is 0.

Description
When FIPSMode is enabled, InterSystems IRIS® data platform uses the FIPS 140–2 compliant library for database encryption on Red Hat Enterprise Linux 6.6 (or later minor version) and Red Hat Enterprise Linux 7.1 (or later minor version) for x86-64.

Note:

Enabling FIPSMode only affects encrypted databases. To encrypt a database, choose the Encrypt Database? option during the database creation process.

See the article FIPS 140–2 Compliance for Database Encryption for details.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the FIPSMode row, select Edit. Select FIPSMode to use the FIPS 140-2 compliant library for database encryption.

Instead of using the Management Portal, you can change FIPSMode in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

IPv6

IPv6

Allow InterSystems IRIS® data platform to accept IPv6 addresses.

Synopsis

[Startup] IPv6=n

n is either 1 (true) or 0 (false). The default value is 0.

Description
IPv6 controls whether your instance is operating in an IPv6 (Internet Protocol Version 6) network, with IPv6 addresses. For more information, see IPv6 Support.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the IPv6 row, select Edit. Select IPv6 to enable this setting.

Instead of using the Management Portal, you can change IPv6 in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[Startup]

JobHalt

Allow custom routines during background process (job) shutdown.

Synopsis

[Startup] JobHalt=n

n is either 1 (true) or 0 (false). The default value is 1.

Description
When JobHalt is enabled (n = 1), InterSystems IRIS® data platform executes the JOB^%ZSTOP routine entry when a background process ends. Background processes include any processes that are started via the JOB command, plus any background server processes including ODBC or any of the language bindings. When this parameter is not enabled, the routine is not executed.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the JobHalt row, select Edit. Select JobHalt to enable this setting.

Instead of using the Management Portal, you can change JobHalt in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

ProcessStart and ProcessHalt correspond to foreground processes. These are processes that are started via a terminal session or are specifically set to run in the fore ground.

JobStart and JobHalt correspond to background processes. This includes any processes that are started via the JOB command, plus any background server processes including ODBC or any of the object bindings.

SystemStart and SystemHalt correspond to InterSystems IRIS instance startup or shutdown.

CallinStart and CallinHalt correspond to external programs performing a CALLIN.

For more information about all these parameters, see Customizing Start and Stop Behavior with ^%ZSTART and ^%ZSTOP.

JobServers

JobServers

Set the number of job servers.

Synopsis

[Startup] JobServers=n

n is an integer in the range 0—2000. The default value is 0.

Description
The JobServers parameter sets both the target number of available job servers you want to maintain and the number that are created at startup. It is not the total number of job servers. When the number of available job servers falls below the target number, more job servers are created in order to maintain the number of available job servers.

Having a large number of job servers running will use more memory and processes, but allows for much faster jobbing of processes because InterSystems IRIS doesn't have to start the processes at the system level and then initialize them.

You can use the ^JOBSRV routine to display information about job servers including the configuration and the number of job servers.

System Behavior
When job servers are enabled, the system runs a monitor process every 5 seconds to determine the number of available job servers. If the number of available job servers is below the target number, more job servers are created until the target is reached.

Every 3 minutes, the monitor also checks if the number of available job servers exceeds the target number. If it does, it stops a single job server. However, the total number of running job servers will never be decreased to fewer than the number set by the JobServers parameter.

If the JobServers parameter is changed to 0 (disabling job servers), the monitor immediately stops all available job servers and continues to stop utilized job servers as they become available.

When utilized job servers complete their work, they are automatically returned to the available pool.

Determining the Target Number The system will maintain a number of available job servers based on an effective target number. The effective target number is determined by whichever is smaller: the parameter or the dynamic target number determined by the total number of job servers (see the table below). For example, if you set JobServers=7, the effective target number will be 5 when there are fewer than 20 total job servers and 7 when there are 20 or more. The examples below illustrate this in more detail.

Total Number of Job Servers

Dynamic Target Number

100+

determined by the parameter

[Startup]

Examples

Parameter: JobServers=4

Every time a new process starts, it uses one of the available job servers, so the system creates a new job server.

Parameter

Dynamic
Target

Effective
Target

Available

Utilized

Total

## 1 process

## 5 processes

Parameter: JobServers=12

N/A

N/A

N/A

The system initially starts with 12 available job servers, but an effective target of 5. When the number of available job servers drops below 5, more are created to maintain the effective target number.

When 15 of the job servers are utilized, the system reaches 20 total job servers (15 utilized and 5 available). This causes the effective target to increase to 10, so the system creates 5 additional job servers. The system now has
## 25 total job servers (15 utilized and 10 available).

When 90 of the job servers are utilized, the system reaches 100 total job servers (90 utilized and 10 available). This causes the dynamic target to increase to 20. Since the parameter is now less than the dynamic target, it becomes the effective target. The system only creates 2 additional job servers in order to meet the effective target of 12.

Parameter

Dynamic
Target

Effective
Target

Available

Utilized

Total

processes

## 15 processes

## 30 processes

## 90 processes

## 100 processes

Parameter: JobServers=100

The system initially starts with 100 available job servers, but an effective target of 20. Once the number of available job servers is reduced to the effective target number (as they are utilized), the system begins creating new job servers to maintain the number available.

JobServers

Parameter

Dynamic
Target

Effective
Target

Available

Utilized

Total

## 50 processes

## 80 processes

## 90 processes

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the JobServers row, select Edit. Enter a number of job servers.

Instead of using the Management Portal, you can change JobServers in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[Startup]

JobStart

Allow custom routines during background process (job) startup.

Synopsis

[Startup] JobStart=n

n is either 1 (true) or 0 (false). The default value is 1.

Description
When JobStart is enabled (n = 1), InterSystems IRIS® data platform executes the JOB^%ZSTART routine entry when a background process starts. Background processes include any processes that are started via the JOB command, plus any background server processes including ODBC or any of the language bindings. When this parameter is not enabled, the routine is not executed.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the JobStart row, select Edit. Select JobStart to enable this setting.

Instead of using the Management Portal, you can change JobStart in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

ProcessStart and ProcessHalt correspond to foreground processes. These are processes that are started via a terminal session or are specifically set to run in the fore ground.

JobStart and JobHalt correspond to background processes. This includes any processes that are started via the JOB command, plus any background server processes including ODBC or any of the object bindings.

SystemStart and SystemHalt correspond to InterSystems IRIS instance startup or shutdown.

CallinStart and CallinHalt correspond to external programs performing a CALLIN.

For more information about all these parameters, see Customizing Start and Stop Behavior with ^%ZSTART and ^%ZSTOP.

LicenseID

LicenseID

Allow instance to request a key from the license server.

Synopsis

[Startup] LicenseID=n

n is the name of a license key in the target .key file. By def ault, no key is specified.

Description
If InterSystems IRIS® data platform does not detect a local iris.key file at instance startup, it uses LicenseID to request a license key from the License Server. Each license key loaded in the License Server will have a unique LicenseID.

For more information about license keys, see Managing InterSystems IRIS Licensing.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the LicenseID row, select Edit. Enter a LicenseID.

Instead of using the Management Portal, you can change LicenseID in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[Startup]

MaxConsoleLogSize

Set the maximum size of the messages.log file.

Synopsis

[Startup] MaxConsoleLogSize=n

n is an integer in the range 1—500 (MB). The default value is 5.

Description
MaxConsoleLogSize is the maximum size of the InterSystems IRIS® data platform console file, in me gabytes. The console file is messages.log, located in the install-dir\mgr directory by default, or the directory specified by the console parameter.

If you enter a value that is smaller than the current setting of MaxConsoleLogSize, or if the console file gro ws to reach the size limit, then the current messages.log file is renamed to messages.old_Date. The instance creates an empty messages.log file, and ne w entries are appended to the newly-created file.

You can view the messages log on Messages Log page of the Management Portal (System Operation > System Logs > Messages Log). To configure the location of the messages.log file, see ConsoleFile parameter.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the MaxConsoleLogSize row, select Edit. Enter a number of megabytes.

Instead of using the Management Portal, you can change MaxConsoleLogSize in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

MaxIRISTempSizeAtStart

MaxIRISTempSizeAtStart

Set the maximum size of the IRISTEMP database on restart.

Synopsis

[Startup] MaxIRISTempSizeAtStart=n

n is an integer in the range 0—1,000,000 (MB). The default value is 0.

Description
MaxIRISTempSizeAtStart is the maximum physical size in megabytes of the IRISTEMP database during instance startup. When the instance restarts, InterSystems IRIS® data platform truncates the IRISTEMP database to the size that MaxIRISTempSizeAtStart specifies. If this parameter is set to 0, the restart.

IRISTEMP database is not truncated on instance

Note:

InterSystems IRIS initializes a maximum of 240 MB in the IRISTEMP database at instance startup. This may be significantly less than the ph ysical size of the database’s IRIS.DAT file.

You can review the IRISTEMP free space information to see the logical size of IRISTEMP, and manually truncate the database from the Management Portal if IRISTEMP grows too large.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the MaxIRISTempSizeAtStart row, select Edit. Enter a number of megabytes.

Instead of using the Management Portal, you can change MaxIRISTempSizeAtStart in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[Startup]

PasswordHash

Set the default password for the predefined user accounts using a cryptographic hash of the passw ord and its salt.

Synopsis

[Startup] PasswordHash=a,b[,c,d]

a is a hashed password. b is its salt, c and d optionally indicate the work factor and algorithm used to hash the password.

Description
The primary use for PasswordHash is to set the default password for the predefined user accounts in automated deployments. PasswordHash specifies a hashed passw ord and its salt. and optionally the work factor (default 10000) and the hashing algorithm (default SHA512) used to hash the password. When it is used in a CPF merge file, on instance startup the stored password hash for each enabled user account with at least one assigned role — that is, all of the predefined accounts e xcept CSPSystem, which does not have an assigned role — is set to the value of PasswordHash . If PasswordHash is empty, the CPF merge operation ignores it.

Important:

The PasswordHash property can be used just once on any given InterSystems IRIS instance, and only if the default password has not yet been changed for any of the predefined accounts. Because allo wing the default password to remain unchanged following deployment is a serious security risk, the PasswordHash setting should be used in a configuration mer ge operation to change the default password during deployment and not later. (For information on how to change an individual user’s password, see Edit an Existing User
Account.)

PasswordHash is not recommended for changing passwords on a deployed instance.

Example

For details about hashing a password and an example of using PasswordHash when deploying a container, see Authentication and Passwords.

- Authentication and Passwords

- Instance Authentication

- Editing an Existing User Account

ProcessHalt

ProcessHalt

Allow custom routines during foreground process shutdown.

Synopsis

[Startup] ProcessHalt=n

n is either 1 (true) or 0 (false). The default value is 1.

Description
When ProcessHalt is enabled (n = 1), InterSystems IRIS® data platform executes the LOGIN^%ZSTOP routine entry at foreground process logout (such as when a user closes the terminal). When this parameter is not enabled, the routine is not executed.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the ProcessHalt row, select Edit. Select ProcessHalt to enable this setting.

Instead of using the Management Portal, you can change ProcessHalt in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

ProcessStart and ProcessHalt correspond to foreground processes. These are processes that are started via a terminal session or are specifically set to run in the fore ground.

JobStart and JobHalt correspond to background processes. This includes any processes that are started via the JOB command, plus any background server processes including ODBC or any of the object bindings.

SystemStart and SystemHalt correspond to InterSystems IRIS instance startup or shutdown.

CallinStart and CallinHalt correspond to external programs performing a CALLIN.

For more information about all these parameters, see Customizing Start and Stop Behavior with ^%ZSTART and ^%ZSTOP.

[Startup]

ProcessStart

Allow custom routines during foreground process startup.

Synopsis

[Startup] ProcessStart=n

n is either 1 (true) or 0 (false). The default value is 1.

Description
When ProcessStart is enabled (n = 1), InterSystems IRIS® data platform executes the LOGIN^%ZSTART routine entry at foreground process login (such as when a user logs in to the terminal). When this parameter is not enabled, the routine is not executed.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the ProcessStart row, select Edit. Select ProcessStart to enable this setting.

Instead of using the Management Portal, you can change ProcessStart in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

ProcessStart and ProcessHalt correspond to foreground processes. These are processes that are started via a terminal session or are specifically set to run in the fore ground.

JobStart and JobHalt correspond to background processes. This includes any processes that are started via the JOB command, plus any background server processes including ODBC or any of the object bindings.

SystemStart and SystemHalt correspond to InterSystems IRIS instance startup or shutdown.

CallinStart and CallinHalt correspond to external programs performing a CALLIN.

For more information about all these parameters, see Customizing Start and Stop Behavior with ^%ZSTART and ^%ZSTOP.

ShutdownTimeout

ShutdownTimeout

Set the number of seconds InterSystems IRIS® data platform should wait until forcing a shutdown.

Synopsis

[Startup] ShutdownTimeout=n

n is an integer in the range 120—100,000. The default value is 300 (5 minutes).

Description
ShutdownTimeout is the number of seconds InterSystems IRIS should wait for shutdown to complete normally before timing out and forcing a shutdown.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the ShutdownTimeout row, select Edit. Enter a number of seconds.

Instead of using the Management Portal, you can change ShutdownTimeout in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[Startup]

SystemHalt

Allow custom routines during instance shutdown.

Synopsis

[Startup] SystemHalt=n

n is either 1 (true) or 0 (false). The default value is 1.

Description
When SystemHalt is enabled (n = 1), InterSystems IRIS® data platform executes the SYSTEM^%ZSTOP routine entry at instance shutdown. When this parameter is not enabled, the routine is not executed.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the SystemHalt row, select Edit. Select SystemHalt to enable this setting.

Instead of using the Management Portal, you can change SystemHalt in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

ProcessStart and ProcessHalt correspond to foreground processes. These are processes that are started via a terminal session or are specifically set to run in the fore ground.

JobStart and JobHalt correspond to background processes. This includes any processes that are started via the JOB command, plus any background server processes including ODBC or any of the object bindings.

SystemStart and SystemHalt correspond to InterSystems IRIS instance startup or shutdown.

CallinStart and CallinHalt correspond to external programs performing a CALLIN.

For more information about all these parameters, see Customizing Start and Stop Behavior with ^%ZSTART and ^%ZSTOP.

SystemMode

SystemMode

Specify a label that appears in the Management Portal header.

Synopsis

[Startup] SystemMode=n

n is an alphanumeric string. The maximum length is 32 characters. The default is an empty string.

Description
SystemMode defines a label that appears in the Management Portal header. This label can be used to easily identify an instance of InterSystems IRIS® data platform.

You may input any label, though some SystemMode values receive special treatment in the Management Portal. These
values are:

- LIVE — Translates to Live System with red text and a red border.

- TEST — Translates to Test System.

- DEVELOPMENT — Translates to Development System.

- FAILOVER — Translates to Failover System.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the SystemMode row, select Edit. Enter the desired label in the SystemMode text box, then click Save.

Instead of using the Management Portal, you can change SystemMode in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[Startup]

SystemPerformanceDailyReportsOn

Allow daily ^SystemPerformance report to run in the background.

Synopsis

[Startup] SystemPerformanceDailyReportsOn=n

n is either 0 (false), 1 (true), or is a minus sign (-) followed by an integer, for example -N.

The default value is 0.

Set n to 1 to enable the daily ^SystemPerformance report, it will run with the default collection interval of 10 seconds.

Set n to -N (with N in seconds) to enable the daily ^SystemPerformance report and change its collection interval to the value passed to N. For example, -20 enables the report and has a 20 second collection interval. The allowed range of collection intervals is -300 to -2.

Description
When SystemPerformanceDailyReportsOn is enabled, InterSystems IRIS® data platform automatically runs a daily ^SystemPerformance report in the background. This daily report runs for 24 hours and ends at midnight each day, where it is then collected and a new report is started. When SystemPerformanceDailyReportsOn is enabled and you restart InterSystems IRIS, a new report will start and the old report is collected.

If SystemPerformanceDailyReportsOn is enabled and then changed to disabled, InterSystems IRIS will stop running the report and collect the running report.

The reports collected when SystemPerformanceDailyReportsOn is enabled are saved to the InterSystems IRIS system manager directory (install-dir/mgr) and the reports are compressed by default.

If SystemPerformanceDailyReportsOn is enabled and you change the collection interval to a new value, the new interval will apply in the next report after the current report completes at midnight.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the SystemPerformanceDailyReportsOn row, select Edit. Enter the desired value in the SystemPerformanceDailyReportsOn text box, then click Save.

Instead of using the Management Portal, you can change SystemPerformanceDailyReportsOn in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

SystemPerformanceDailyReportsRetentionDays

SystemPerformanceDailyReportsRetentionDays

Set the number of days before InterSystems IRIS® data platform purges the daily ^SystemPerformance reports.

Synopsis

[Startup] SystemPerformanceDailyReportsRetentionDays=n

n is an integer. The default value is 60. Set n to 0 or leave empty to disable all purging and keep all reports.

Description
SystemPerformanceDailyReportsRetentionDays defines the number of days to retain the daily ^SystemPerformance reports, generated via the SystemPerformanceDailyReportsOn setting, before InterSystems IRIS purges them. The value of n is the number of days that the reports are retained before being purged.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the SystemPerformanceDailyReportsRetentionDays row, select Edit. Enter the desired number of days in the SystemPerformanceDailyReportsRetentionDays text box, then click Save.

Instead of using the Management Portal, you can change SystemPerformanceDailyReportsRetentionDays in the Config.Startup class (as described in the class reference) or by editing the CPF in a te xt editor (as described in Editing the Active CPF).

[Startup]

SystemStart

Allow custom routines during instance startup.

Synopsis

[Startup] SystemStart=n

n is either 1 (true) or 0 (false). The default value is 1.

Description
When SystemStart is enabled (n = 1), InterSystems IRIS® data platform executes the SYSTEM^%ZSTART routine entry at instance startup. When this parameter is not enabled, the routine is not executed.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the SystemStart row, select Edit. Select SystemStart to enable this setting.

Instead of using the Management Portal, you can change SystemStart in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

ProcessStart and ProcessHalt correspond to foreground processes. These are processes that are started via a terminal session or are specifically set to run in the fore ground.

JobStart and JobHalt correspond to background processes. This includes any processes that are started via the JOB command, plus any background server processes including ODBC or any of the object bindings.

SystemStart and SystemHalt correspond to InterSystems IRIS instance startup or shutdown.

CallinStart and CallinHalt correspond to external programs performing a CALLIN.

For more information about all these parameters, see Customizing Start and Stop Behavior with ^%ZSTART and ^%ZSTOP.

TempDirectory

TempDirectory

Specify the subdirectory for temporary files.

Synopsis

[Startup] TempDirectory=n

n is an absolute or relative directory pathname. Provided that the value of TempDirectory is a valid pathname, there is no maximum length. The default is Temp, which corresponds to <install-dir>\mgr\Temp.

Description
TempDirectory is the name of the subdirectory for InterSystems IRIS® data platform to store temporary files. When you set a new TempDirectory value, the instance creates a subdirectory of this name which becomes the new InterSystems IRIS temporary directory.

You can specify a full or relative path. If you specify a full path, InterSystems IRIS uses the specified directory . If you specify a relative path, InterSystems IRIS creates the directory under the <install-dir>\mgr\ subdirectory.

Examples

To create c:\InterSystems\iris\mgr\Temp\ on Windows:

TempDirectory=Temp

To create c:\TempFiles\ on Windows:

TempDirectory=c:\TempFiles

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the TempDirectory row, select Edit. Enter a subdirectory name.

Instead of using the Management Portal, you can change TempDirectory in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[Startup]

TerminalPrompt

Define the format of the terminal prompt.

Synopsis

[Startup] TerminalPrompt=n

n is a string of comma-separated values (0–8) that set the Terminal prompt. The default is 8,2.

Description
TerminalPrompt configures the information displayed in the Terminal prompt for the instance. The order of the values in the string determines the order the information appears in the prompt. For example, the string 2,1 produces a terminal prompt of %SYS:HostName>.

The values are coded as follows:

- 0 - Simple prompt. Specify 0 with no other values to use only “ >” for the prompt.

- 1 - Host name, also known as the current system name. The name assigned to your computer. For example, LABLAPTOP>. This is the same for all of your terminal processes.

- 2 - Namespace name. For example, %SYS>. The current namespace name is contained in the $NAMESPACE special
variable. It can be an explicit namespace name or an implied namespace name.

- 3 - Config name. The name of your instance. For example, IRIS2>. This is the same for all of your terminal processes.

- 4 - Current time, expressed as local time in 24-hour format with whole seconds. For example, 15:59:36>. This is the static time value for when the prompt was returned. This value changes for each prompt.

- 5 - The Process ID for your terminal. For example, 2336>. This is different for each terminal process. This value can
also be returned from the $JOB special variable.

- 6 - Username. For example, fred>. This is the same for all of your terminal processes.

- 7 - Time elapsed executing the last command, in seconds.milliseconds. For example, .000495>. Leading and trailing zeros are suppressed. This changes for each prompt.

- 8 - Transaction Level. For example, TL1>.

TerminalPrompt cannot be left empty. If the input string is invalid, the Terminal uses the default value for TerminalPrompt: 8,2.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the TerminalPrompt row, select Edit. Enter a comma-separated string of values, or 0.

Instead of using the Management Portal, you can change TerminalPrompt in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

WebServer

WebServer

Allow the private web server to start, if one is present.

Synopsis

Important:

Versions of InterSystems IRIS® prior to 2023.2 included a private web server that served built-in web applications such as the Management Portal. Beginning with 2023.2, new installations of InterSystems IRIS no longer include a private web server. See this section of the Web Gateway Guide for more information.

[Startup] WebServer=n

n is either 1 (true) or 0 (false). The default value is 0 for new installations, 1 for upgrades.

Description
When WebServer is enabled (n = 1), InterSystems IRIS attempts to start the Apache private web server upon startup.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the WebServer row, select Edit. Select WebServer to enable the private web server.

Instead of using the Management Portal, you can change WebServer in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

Note:

A restart is required for this setting to take effect.

- WebServerName

- WebServerPort

- WebServerProtocol

- WebServerURLPrefix

[Startup]

WebServerName

Identify an IP address or a DNS name for the web server.

Synopsis

[Startup] WebServerName=nnn.nnn.nn.nn (ip address) or www.DNSname.com

nnn.nnn.nn.nn is a valid IP address. www.DNSname.com is a valid DNS name. Only specify one value.

Description

Important:

Note that this parameter is only used in conjunction with Studio, which has been removed as of the 2024.2 release. See Deprecated and Discontinued Features for more information.

WebServerName identifies the DNS name or the IP address of the web serv er that is configured for use with InterSystems IRIS® data platform tools. It is necessary to set this parameter only if you must enable InterSystems Studio.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the WebServerName row, select Edit. Enter a DNS name or an IP address.

Instead of using the Management Portal, you can change WebServerName in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

- WebServer

- WebServerPort

- WebServerProtocol

- WebServerURLPrefix

WebServerPort

WebServerPort

Identify the web server port.

Synopsis

[Startup] WebServerPort=nnnnn

n is a valid port number. The default is 80.

Description

Important:

Note that this parameter is only used in conjunction with Studio, which has been removed as of the 2024.2 release. See Deprecated and Discontinued Features for more information.

WebServerPort identifies the port number of the web serv er that is configured for use with InterSystems IRIS® data platform tools. It is necessary to set this parameter only if you must enable InterSystems Studio.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the WebServerPort row, select Edit. Enter a port number.

Instead of using the Management Portal, you can change WebServerPort in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

- WebServer

- WebServerName

- WebServerProtocol

- WebServerURLPrefix

[Startup]

WebServerProtocol

Enable InterSystems IRIS to receive HTTPS connections over a custom port.

Synopsis

[Startup] WebServerProtocol=protocol

protocol is either http (the default) or https.

Description

Important:

Note that this parameter is only used in conjunction with Studio, which has been removed as of the 2024.2 release. See Deprecated and Discontinued Features for more information.

When WebServerPort is set to 443 (the standard port for HTTPS), Studio automatically uses HTTPS. However, if it is set to some other value, WebServerProtocol must be set to https to enable Studio to communicate over HTTPS. It is necessary to set this parameter only if you must enable InterSystems Studio.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the WebServerProtocol row, select Edit. Enter https or http.

Instead of using the Management Portal, you can change WebServerProtocol in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

- WebServer

- WebServerName

- WebServerPort

- WebServerURLPrefix

WebServerSSLConfiguration

Specify the default TLS configuration for %Net.HttpRequest objects created by CSP templates implemented in Studio.

WebServerSSLConfiguration

Synopsis

[Startup] WebServerSSLConfiguration=configuration

configur ation is the location of a TLS configuration

Description

Important:

This parameter has been retained for compatibility, but should not be used when building new applications.

Deprecated. Must be specified when using Studio to create a web application from a CSP template on an instance with an active private web server that accepts only HTTPS connections.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the WebServerProtocol row, select Edit. Enter the location of the TLS configuration.

Instead of using the Management Portal, you can change WebServerSSLConfiguration in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

- WebServer

- WebServerName

- WebServerPort

- WebServerProtocol

- WebServerURLPrefix

[Startup]

WebServerURLPrefix

Identify the instance prefix that the web serv er uses to access the InterSystems IRIS® data platform.

Synopsis

[Startup] WebServerURLPrefix=n

n is an alphanumeric string to be used in a URL. As a guideline, WebServerURLPrefix should be shorter than 80 characters. The default is the instance name in all lowercase characters.

Description

Important:

Note that this parameter is only used in conjunction with Studio, which has been removed as of the 2024.2 release. See Deprecated and Discontinued Features for more information.

WebServerURLPrefix is used by Studio when constructing URLs. It is necessary to set this parameter only if you must enable InterSystems Studio. This should match the CSP Server Instance setting.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the WebServerURLPrefix row, select Edit. Enter an InterSystems IRIS instance name.

Instead of using the Management Portal, you can change WebServerURLPrefix in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

- WebServer

- WebServerName

- WebServerPort

- WebServerProtocol

ZSTU

ZSTU

Allow the user-defined startup to run.

Synopsis

[Startup] ZSTU=n

n is either 1 (true) or 0 (false). The default value is 1.

Description

Important:

This parameter has been retained for compatibility, but the ^ZSTU routine is no longer recommended for
use; use the ^%ZSTART routine instead.

When ZSTU is enabled (n = 1), InterSystems IRIS® data platform runs the user-defined startup from the ^ZSTU routine.

On the Startup page of the Management Portal (System Administration > Configuration > Additional Settings > Startup), in the ZSTU row, select Edit. Select ZSTU to enable this setting.

Instead of using the Management Portal, you can change ZSTU in the Config.Startup class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[Telnet]

This topic describes the parameters found in the [Telnet] section of the CPF. These settings only apply to Windows configurations, in which InterSystems supplies the Telnet servers. They do not apply to UNIX® or Linux configurations, in which Telnet is supplied by the operating system vendor.

You can also find these settings on the Telnet Settings page of the Management Portal (System Administration > Configuration > Device Settings > Telnet Settings).

[Telnet]

DNSLookup

Allow DNS lookup of the Telnet client address. Windows systems only.

Synopsis

[Telnet] DNSLookup=n

n is either the string ON or OFF. The default value is ON.

Description
DNSLookup enables or disables DNS lookup of the client address in the telnet daemon before passing the address to the InterSystems IRIS® data platform process that was created to service the connection. This determines the format of the
client address returned by $IO and $ZIO in the InterSystems IRIS process.

When DNSLookup is enabled, a DNS lookup of the client address is performed, and the client name is passed to InterSystems IRIS. When DNSLookup is not enabled, no DNS lookup is performed, and the client address is provided in either dotted decimal format (if the connection was via IPV4) or in the colon separated hexadecimal format (if the connection was via IPV6). You should disable this parameter if a DNS server is not available to do the lookup, because a long delay will occur during login if the DNS server is not available.

InterSystems IRIS Telnet settings apply only to Windows configurations in which InterSystems supplies the Telnet servers. This parameter is ignored for UNIX® systems.

On the Telnet Settings page of the Management Portal (System Administration > Configuration > Device Settings > Telnet Settings), in the DNS Lookup field, choose ON or OFF.

Instead of using the Management Portal, you can change DNSLookup in the Config.T elnet class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

If you edit this setting, you must restart InterSystems IRIS to apply the change.

Port

Port

Set the Telnet port number. Windows systems only.

Synopsis

[Telnet] Port=n

n is a valid TCP/IP port number. The default is value 23.

Description
Port is the TCP/IP port number for Telnet connections. If multiple InterSystems IRIS® data platform configurations are to run on the same host at the same time, a different Telnet port number must be specified for each configuration. Clients can attach to configurations using the non-def ault port number by specifying the port number when they invoke Telnet on
the client system. Telnet, with or without SSL, can be configured on an y port; it does not require the use of port 992.

InterSystems IRIS Telnet settings apply only to Windows configurations in which InterSystems supplies the Telnet servers. This parameter is ignored for UNIX® systems.

On the Telnet Settings page of the Management Portal (System Administration > Configuration > Device Settings > Telnet Settings), in the Telnet Port Number field, enter a TCP/IP port number.

Instead of using the Management Portal, you can change Port in the Config.T elnet class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

[WorkQueues]

This topic describes the work queue manager categories found in the [WorkQueues] section of the CPF. You may create a new category using the same syntax as an existing category. For more information about the Work Queue Manager, see Using the Work Queue Manager.

[WorkQueues]

Default

Define the “Default” work queue manager category.

Synopsis

[WorkQueues] Default=MaxActiveWorkers,DefaultWorkers,MaxWorkers,MaxTotalWorkers,AlwaysQueue

Default is the name of this work queue manager category. MaxActiveWorkers, DefaultWorkers, MaxWorkers, MaxTotalWorkers, and AlwaysQueue are four comma-separated integers and one comma-separated boolean. Any of these may be left blank or specified as 0 to use the default value. For more information on each property, see Category Properties.

Description
The [WorkQueues] section of the configuration parameter file (CPF) contains an entry for e
category. A category has up to fiv e comma-separated properties. None of the arguments are required; if any are left blank,
they are resolved as the default value. For more information, see Managing Categories.

very work queue manager

On the Work Queue Manager Categories page of the Management Portal (System Administration > Configuration > System Configuration > WQM Categories), click Default to edit this category. you can instead click Create Category to create a new WQM category.

Instead of using the Management Portal, you can change the Default category in the Config.W orkQueues class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

- Creating a Work Queue Manager Category

- SQL

SQL

Define the “SQL” work queue manager category.

Synopsis

[WorkQueues] SQL=MaxActiveWorkers,DefaultWorkers,MaxWorkers,MaxTotalWorkers,AlwaysQueue

SQL is the name of this work queue manager category. MaxActiveWorkers, DefaultWorkers, and MaxWorkers, MaxTotalWorkers, and AlwaysQueue are four comma-separated integers and one comma-separated boolean. Any of these may be left blank or specified as 0 to use the default value. For more information on each property, see Category Properties.

Description
The [WorkQueues] section of the configuration parameter file (CPF) contains an entry for e
category. A category has up to fiv e comma-separated properties. None of the arguments are required; if any are left blank,
they are resolved as the default value. For more information, see Managing Categories.

very work queue manager

On the Work Queue Manager Categories page of the Management Portal (System Administration > Configuration > System Configuration > WQM Categories), click SQL to edit this category. you can instead click Create Category to create a new WQM category.

Instead of using the Management Portal, you can change the SQL category in the Config.W orkQueues class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

- Configuring Work Queue Manager Categories

- [WorkQueues]

Utility

Define the “Utility” work queue manager category.

Synopsis

[WorkQueues] Utility=MaxActiveWorkers,DefaultWorkers,MaxWorkers,MaxTotalWorkers,AlwaysQueue

Utility is the name of this work queue manager category. MaxActiveWorkers, DefaultWorkers, and MaxWorkers, MaxTotalWorkers, and AlwaysQueue are four comma-separated integers and one comma-separated boolean. MaxActiveWorkers, DefaultWorkers, and MaxWorkers, and MaxTotalWorkers may be left blank or specified as 0 to use
the default value; the default value of AlwaysQueue for the Utility category is 1. For more information on each property,
see Category Properties.

Description
The [WorkQueues] section of the configuration parameter file (CPF) contains an entry for e
category. A category has up to fiv e comma-separated properties. None of the arguments are required; if any are left blank,
they are resolved as the default value. For more information, see Managing Categories.

very work queue manager

On the Work Queue Manager Categories page of the Management Portal (System Administration > Configuration > System Configuration > WQM Categories), click Utility to edit this category. you can instead click Create Category to create a new WQM category.

Instead of using the Management Portal, you can change the Utility category in the Config.W orkQueues class (as described in the class reference) or by editing the CPF in a text editor (as described in Editing the Active CPF).

- Configuring Work Queue Manager Categories
