---
title: Web UI Overview
---
# Accessing the Web UI

Users can access the Web UI to interact with the underlying Java application running inside the WebSphere Liberty Profile JVM server inside CICS using REST principles.

# Web UI Components

The Web UI includes various static assets and configuration files such as <SwmPath>[src/bank-application-frontend/public/index.html](src/bank-application-frontend/public/index.html)</SwmPath>, <SwmPath>[src/bank-application-frontend/public/manifest.json](src/bank-application-frontend/public/manifest.json)</SwmPath>, and <SwmPath>[src/webui/WebContent/asset-manifest.json](src/webui/WebContent/asset-manifest.json)</SwmPath>.

# Web UI Endpoints

The Web UI interacts with the backend through various endpoints. These endpoints are defined in Java classes and use REST principles to communicate with the backend.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/SortCodeResource.java" line="54">

---

## <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/SortCodeResource.java" pos="56:5:5" line-data="	public Response getSortCode()">`getSortCode`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/SortCodeResource.java" pos="56:5:5" line-data="	public Response getSortCode()">`getSortCode`</SwmToken> endpoint retrieves the sort code from the backend. It checks if the sort code is cached; if not, it calls a COBOL program to fetch it and then caches it.

```java
	@GET
	@Produces("application/json")
	public Response getSortCode()
	{

		if (sortCodeString == null)
		{
			Program getscode = new Program();
			getscode.setName("GETSCODE");

			byte[] sortCodeBytes = new byte[6];

			try
			{
				getscode.link(sortCodeBytes);
			}
			catch (InvalidRequestException | LengthErrorException
					| InvalidSystemIdException | NotAuthorisedException
					| InvalidProgramIdException | RolledBackException
					| TerminalException e)
			{
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" line="130">

---

## <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="133:5:5" line-data="	public Response getCustomerCounter()">`getCustomerCounter`</SwmToken>

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CounterResource.java" pos="133:5:5" line-data="	public Response getCustomerCounter()">`getCustomerCounter`</SwmToken> endpoint retrieves the current customer counter. It calls a COBOL program to get the counter value and returns it in JSON format.

```java
	@GET
	@Path("/customer")
	@Produces("application/json")
	public Response getCustomerCounter()
	{
		logger.entering(this.getClass().getName(), GET_CUSTOMER_COUNTER);

		JSONObject response = new JSONObject();
		Response myResponse = null;

		Program newcusnoProgram = new Program();
		newcusnoProgram.setName(NEWCUSNO);

		NewCustomerNumber myNEWCUSNO = new NewCustomerNumber();

		myNEWCUSNO.setNewcusnoFunction("C");
		byte[] data = myNEWCUSNO.getByteBuffer();
		try
		{
			newcusnoProgram.link(data);
			myNEWCUSNO = new NewCustomerNumber(data);
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=" repo-name="cics-banking-sample-application-cbsa-IBM-Demo-GPT"><sup>Powered by [Swimm](/)</sup></SwmMeta>
