# Microsoft Internet Information Services Web Server

Web Server Considerations (Windows)

This page provides additional technical details which you may wish to consider as you deploy an InterSystems Web Gateway alongside a Microsoft Internet Information Services (IIS) web server. Refer to IIS documentation for further details about IIS administration: https://learn.microsoft.com/en-us/iis/get-started/introduction-to-iis/iis-web-server-overview.

## 1 Restarting IIS

IIS must be restarted in order for changes to the Web Gateway’s configuration to tak e effect. This must be done by completely restarting the World Wide Web Publishing service from the main Windows Services control panel or through the Windows command line, not through the Internet Services Manager control panel. See Restarting IIS.

## 2 IIS Worker Processes and the Web Gateway

The recommended Web Gateway deployment method (using the Native Modules CSPms.dll and CSPmsSys.dll) manages persistent resources—such as connections to an InterSystems IRIS instance—within the web server extension itself. (By contrast, NSD-based Web Gateway deployments manage resources independently of IIS, within the NSD itself.)

Therefore, the performance of the Web Gateway Native Module extension is influenced by the follo wing IIS configuration
items, which determine how IIS worker processes serve requests for your applications:

Application pool

Within IIS, a designated set of one or more worker processes which can be assigned to serve requests for one or more IIS applications.

Web garden

An IIS application pool which contains more than one worker process. Requests are distributed among the worker processes in a web garden.

Each worker process manages its own instance of the Web Gateway extension. For application pools that are configured to use no more than one worker process, this has no visible impact on the way the Web Gateway operates within the context of a single web application path (for example, /csp). However, within a web garden, the requests are evenly distributed amongst the Web Gateway instances managed by participating worker processes.

Because of this, several restrictions must be borne in mind.

### 2.1 Web Gateway System Status

The Web Gateway’s System Status management page cannot accurately monitor the connections used by web applications across an entire web garden. At any given time, the Systems Status page reflects the status for the instance of the Web Gateway that happens to be attached to the current worker process (that is, the worker process that happens to service the Web Gateway’s request).

### 2.2 Minimum and Maximum Connections

Each worker process manages its own instance of the Web Gateway application, and therefore maintains its own pool of persistent connections to InterSystems IRIS application servers. The Web Gateway configuration parameters which specify the minimum and maximum number of InterSystems IRIS connections do not specify those limits in aggregate across an entire application pool. Rather, these parameters define the minimum and maximum number of connections allo wed for each worker process’s Web Gateway instance.

### 2.3 Worker Process Timeout and Worker Process Recycling

You may wish to configure w orker processes within an application pool to recycle periodically or to terminate after a specified period of idle time.

When a worker process within an application pool terminates, the instance of the Web Gateway that the process manages terminates as well, closing the pool of connections that the Web Gateway instance maintained with InterSystems IRIS. Under such circumstances, stateless connections can be replaced in a way that is transparent to users of a web application. However, state-aware sessions (preserve mode 1) terminate when their hosting connection is closed.

## 3 Bitness: Serving a 32-bit Application on a 64-bit Server

Note:

This section applies to modules that are loaded into the address space of the hosting web server: the Native Modules (CSPms[Sys].dll and CSPcms.dll) and the ISAPI extensions. CGI modules are not affected since they run as a process which is detached from IIS.

IIS allows you to set the bitness (64-bit or 32-bit) for a particular application pool. A single IIS installation can feature application pools which serve native 64-bit applications alongside other application pools which serve 32-bit applications. The Enable 32-Bit Applications setting is available by accessing the Advanced Settings for an application pool in the IIS
Manager.

The Native Modules or ISAPI extensions which an application pool loads must match the bitness of the application pool. For example, if the hosting application pool is 64-bit, then the 64-bit Gateway modules (such as CSPms[Sys].dll) must be used. If the hosting application pool is 32-bit, then the 32-bit Gateway modules must be used instead.

The bitness check for individual modules is done via a preCondition in the module’s web.config file. F or the Web Gateway,
this file typically looks something lik e the following:

<?xml version="1.0" encoding="UTF-8"?> <configuration> <system.webServer> <handlers> <add name="WebGateway_All" path="*" verb="*" modules="CSPms" resourceType="Unspecified" \ preCondition="bitness64" /> </handlers> <security> <requestFiltering> <hiddenSegments> <remove segment="bin" /> </hiddenSegments> </requestFiltering> </security> </system.webServer> </configuration>

Note the bitness setting in the precondition clause. In this case bitness is set to bitness64 which means that IIS checks for 64-bit Gateway modules operating in a 64-bit Application Pool.

If a 32-bit Application Pool is used, then the 32-bit Gateway modules must be used and the preCondition set to bitness32.

If there is an inconsistency between the modules installed, the precondition clause, or the expectations of the hosting Application Pool, IIS returns an error.
