# Using Java Messaging Service (JMS) in Interoperability

Important:

This page discusses a legacy implementation of JMS messaging that should not be used for new implementations. This legacy implementation may be removed in future releases. Instead, use the JMSPEX interoperability adapters (inbound and outbound) which InterSystems has implemented using the PEX framework. Alternatively, use the JMS Messaging API.

The Java Messaging Service (JMS) is a Java messaging framework for providing communication between two or more systems. In this framework, a JMS provider manages a queue of messages sent by JMS clients. A typical JMS message has
the following path:

1. A JMS client sends the message to a JMS provider.

2. The JMS provider sends the message to another JMS client.

With interoperability productions, InterSystems products can be a JMS client that both sends and receives JMS messages. InterSystems JMS clients use the EnsLib.JMS.Operation business host to send messages to JMS providers and the EnsLib.JMS.Service business host to receive messages from JMS providers. Advanced users who are familiar with ObjectScript can create their own custom JMS business hosts rather than using these built-in components.

Internally, JMS business hosts leverage an InterSystems external server to connect to Java.

### 1.1 JMS Messages

Within the JMS client’s interoperability production, the JMS messages are instances of EnsLib.JMS.Message. The text property of message object contains the message content. The type property of the message object specifies the message type such as TextMessage and BytesMessage. The EnsLib.JMS.Message class also provides methods for setting and retrieving properties of the message.

### 1.2 Jar Files

The jar file for the JMS feature is a vailable at: install-dir\dev\java\lib\JDK18\intersystems-enslib-jms-3.0.0.jar

The following client development jar files are also a vailable:

- install-dir\dev\java\jms\wljmsclient.jar

- install-dir\dev\java\jms\wlthint3client.jar Using JMS Business Services and Operations (Legacy Implementation)

Important:

This page discusses a legacy implementation of JMS messaging that should not be used for new implementations. This legacy implementation may be removed in future releases. Instead, use the JMSPEX interoperability adapters (inbound and outbound) which InterSystems has implemented using the PEX framework.

To enable an InterSystems production to receive JMS messages, add a new business service, specifying the service class of this business service as EnsLib.JMS.Service. This business service ignores any response.

To enable an InterSystems production to send JMS messages, add a new business operation, specifying the operation class of this business operation as EnsLib.JMS.Operation. This business operation returns an instance of EnsLib.JMS.Response the business host that sent the JMS message to the business operation.

Once you have added these business hosts to the production, configure the follo wing settings on the Settings tab:

- JMSCredentials—The credential defined for the username and passw ord of the JMS server. See Defining Credentials .

- JavaGatewayHost and JavaGatewayPort—The IP address and port of the InterSystems external language server for Java. See Managing External Server Connections.

- JMS Server—URL of the JMS server.

- JMSFactory—Name of the QueueConnectionFactory.

- JMSQueue—Name of the JMS Queue.

- JMSClientID—Name that appears on the JMS Server’s list of active connections.

Creating Custom JMS Services and Operations (Legacy Implementation)

Important:

This page discusses a legacy implementation of JMS messaging that should not be used for new implementations. This legacy implementation may be removed in future releases. Instead, use the JMSPEX interoperability adapters (inbound and outbound) which InterSystems has implemented using the PEX framework.

Creating custom JMS business services and business operations requires writing custom ObjectScript code and consequently takes more development resources than using the built-in JMS services and operations, but provides better performance as you can access the Java Gateway proxy object directly.

To develop a custom JMS business service:

- Implement a custom business service class using EnsLib.JMS.InboundAdapter as its adapter.

- Override the OnProcessInput() method with the following signature:

- Method OnProcessInput(pMessage As %Net.Remote.Object, Output pOutput As %RegisteredObject) As %Status pMessage is a Gateway proxy object of a Java message object of class com.intersystems.enslib.jms.Message. Properties and methods of the Java message object can be accessed using the Gateway proxy interface. The pMessage object contains the message received from the JMS provider.

To develop a custom JMS business operation:

- Implement a custom business operation class using EnsLib.JMS.OutboundAdapter as its adapter.

- Override the OnMessage() method or implement a message map. See Defining a Message Map for information on message maps.

- Call ..Adapter.GetNewMessage(tMessage) to get the message that was sent to the business operation by another host in the production.

- tMessage is an instance of %Net.Remote.Object.

- tMessage is a Gateway proxy object of a Java message object of class com.intersystems.enslib.jms.Message. Properties and methods of the Java message object can be accessed using the Gateway proxy interface. Access tMessage with properties and methods that are implemented in Java class com.intersystems.enslib.jms.Message.

Send the message to the JMS provider by calling ..Adapter.SendMessage(tMessage).

Creating Custom JMS Services and Operations (Legacy Implementation)

Once you have developed your custom JMS business service and JMS business operation, you add them to a production just like you would the built-in JMS business hosts.
