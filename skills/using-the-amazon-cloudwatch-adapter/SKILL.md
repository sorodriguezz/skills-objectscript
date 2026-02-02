# Using the Amazon CloudWatch Adapter

Amazon CloudWatch is an AWS product that allows you to monitor an application by collecting data for specific metrics. Once you have created a metric in CloudWatch, you can use a business operation in an InterSystems interoperability production to update CloudWatch with values for that metric. InterSystems provides a built-in business operation that uses the CloudWatch outbound adapter to interact with CloudWatch. You also have the option of creating a custom business operation that uses this adapter. If you are new to interoperability productions, including the use of business operations and outbound adapters, see Introduction to Interoperability Productions.

Important:

Currently, only the PutMetricData method of the CloudWatch adapter is fully functional. Do not attempt to use the PutMetricAlarm method to work with CloudWatch alarms as this method may change in future releases.

## 1 Outbound Adapter Details

The class of the CloudWatch outbound adapter is EnsLib.AmazonCloudWatch.OutboundAdapter. Within this class, the PutMetricData method contains the logic that updates CloudWatch with a value for a specific metric. The signature of
this method is:

Method PutMetricData(namespace As %String, metricName As %String,
metricValue As %Numeric, metricUnit As %String, dims As %String = "") As %Status

Where:

- namespace is the metric's CloudWatch namespace.

- metricName is the name of the metric.

- metricValue is the datapoint being sent to CloudWatch for the specified metric.

- metricUnit is the unit of measure for the metric value. This unit of measure is required. For a list of valid units, see the Amazon CloudWatch JavaDoc reference.

- dims is a JSON array with name/value pairs that represent the metric's dimensions. For example,
[{"Name":"StorageType","Value":"StandardStorage"},{"Name":"BucketName","Value":"test-bazco}]

## 2 Built-in Business Operation

Rather than developing a custom business operation that uses the outbound adapter, you can save time and effort by adding the EnsLib.AmazonCloudWatch.MetricDataOperation business operation to the interoperability production. Once added, the production can send a pre-built request that contains the metric data to the business operation. The class of this pre-built request is EnsLib.AmazonCloudWatch.PutMetricDataRequest.

The business operation contains properties that correspond to the adapter parameters that identify the CloudWatch metric, for example, name and namespace. Once you have added the business operation to the production, you can set these properties using the corresponding Management Portal settings. For instructions on adding a business operation to a production, see Adding Business Hosts.

General AWS Settings

## 3 General AWS Settings

The CloudWatch outbound adapter extends a common adapter class that includes general AWS properties. When you add a business operation that uses the outbound adapter to a production, these AWS properties can be set using the AWS settings in the Management Portal.

CredentialsFile — If blank, Amazon uses the default credential provider chain to obtain the credentials needed to access CloudWatch. If you prefer to use an AWS credential file, enter its filepath.

Region — Identifies the AWS region that you want to access. For a list of CloudWatch regions, see Amazon Regions, Availability Zones, and Local Zones
