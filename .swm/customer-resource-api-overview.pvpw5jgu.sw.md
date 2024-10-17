---
title: Customer Resource API Overview
---
# What is Customer Resource API

The Customer Resource API is a RESTful API endpoint that handles operations related to customer data. It provides methods for creating, reading, updating, and deleting customer records. This API is essential for managing customer information in a structured and efficient manner.

## Creating a Customer

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="54:14:14" line-data="	private static final String CREATE_CUSTOMER_EXTERNAL = &quot;createCustomerExternal(CustomerJSON customer) for customer &quot;;">`createCustomerExternal`</SwmToken> method allows for the creation of a new customer by accepting customer data in JSON format and returning a response. This method ensures that new customer records are added to the system accurately.

## Retrieving Customer Details

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="463:5:5" line-data="	public Response getCustomerExternal(@PathParam(JSON_ID) Long id)">`getCustomerExternal`</SwmToken> method retrieves customer details based on a provided customer ID. This method is crucial for fetching existing customer information efficiently.

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" line="460">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="463:5:5" line-data="	public Response getCustomerExternal(@PathParam(JSON_ID) Long id)">`getCustomerExternal`</SwmToken> method implementation demonstrates how customer data is retrieved and logged based on the customer ID.

```java
	@GET
	@Path("/{id}")
	@Produces(MediaType.APPLICATION_JSON)
	public Response getCustomerExternal(@PathParam(JSON_ID) Long id)
	{
		logger.entering(this.getClass().getName(), GET_CUSTOMER_EXTERNAL + id);

		try
		{
			Response myResponse = getCustomerInternal(id);
			HBankDataAccess myHBankDataAccess = new HBankDataAccess();
			myHBankDataAccess.terminate();
			logger.exiting(this.getClass().getName(), "getCustomerExternal",
					myResponse);
			return myResponse;

		}
		catch (Exception ex)
		{
			// Log the exception
			logger.log(Level.WARNING,
```

---

</SwmSnippet>

<SwmSnippet path="/src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" line="309">

---

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="313:5:5" line-data="	public Response updateCustomerExternal(@PathParam(JSON_ID) Long id,">`updateCustomerExternal`</SwmToken> method implementation shows how customer data is updated and logged based on the customer ID.

```java
	@PUT
	@Path("/{id}")
	@Consumes(MediaType.APPLICATION_JSON)
	@Produces(MediaType.APPLICATION_JSON)
	public Response updateCustomerExternal(@PathParam(JSON_ID) Long id,
			CustomerJSON customer)
	{
		logger.entering(this.getClass().getName(),
				UPDATE_CUSTOMER_EXTERNAL + id);
		Response myResponse = updateCustomerInternal(id, customer);
		HBankDataAccess myHBankDataAccess = new HBankDataAccess();
		myHBankDataAccess.terminate();
		logger.exiting(this.getClass().getName(), UPDATE_CUSTOMER_EXTERNAL + id,
				myResponse);
		return myResponse;
```

---

</SwmSnippet>

## Deleting a Customer

The <SwmToken path="src/webui/src/main/java/com/ibm/cics/cip/bankliberty/api/json/CustomerResource.java" pos="550:5:5" line-data="	public Response deleteCustomerExternal(@PathParam(JSON_ID) Long id)">`deleteCustomerExternal`</SwmToken> method deletes a customer record based on the customer ID. This method is essential for removing outdated or incorrect customer information from the system.

## Listing Customers

Additional methods allow for listing customers by various criteria such as name, town, and age. These methods provide flexibility in retrieving customer data based on different search parameters.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=" repo-name="cics-banking-sample-application-cbsa-IBM-Demo-GPT"><sup>Powered by [Swimm](/)</sup></SwmMeta>
