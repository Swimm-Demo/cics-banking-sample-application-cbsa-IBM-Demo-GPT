---
title: Introduction to Jsonclasses in Customer Services API
---
# Introduction to Jsonclasses in Customer Services API

Jsonclasses are used to define the structure and properties of JSON objects that are exchanged between the client and server in the Customer Services API. These classes ensure that the JSON data received from or sent to the client is correctly mapped to the corresponding Java objects, facilitating seamless data exchange.

## Mapping JSON Properties

In the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/CustomerEnquiryJson.java" pos="42:4:4" line-data="		return &quot;CustomerEnquiryJson [INQCUSTZ=&quot; + INQCUSTZ + &quot;]&quot;;">`CustomerEnquiryJson`</SwmToken> class, the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/CustomerEnquiryJson.java" pos="16:5:5" line-data="	@JsonProperty(&quot;INQCUSTZ&quot;)">`INQCUSTZ`</SwmToken> field is annotated with <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/CustomerEnquiryJson.java" pos="16:1:2" line-data="	@JsonProperty(&quot;INQCUSTZ&quot;)">`@JsonProperty`</SwmToken> to map the JSON property <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/CustomerEnquiryJson.java" pos="16:5:5" line-data="	@JsonProperty(&quot;INQCUSTZ&quot;)">`INQCUSTZ`</SwmToken> to the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/CustomerEnquiryJson.java" pos="17:3:3" line-data="	private InqCustZJson INQCUSTZ;">`InqCustZJson`</SwmToken> object.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/CustomerEnquiryJson.java" line="16">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/CustomerEnquiryJson.java" pos="16:1:2" line-data="	@JsonProperty(&quot;INQCUSTZ&quot;)">`@JsonProperty`</SwmToken> annotation is used to map the JSON property <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/CustomerEnquiryJson.java" pos="16:5:5" line-data="	@JsonProperty(&quot;INQCUSTZ&quot;)">`INQCUSTZ`</SwmToken> to the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/CustomerEnquiryJson.java" pos="17:3:3" line-data="	private InqCustZJson INQCUSTZ;">`InqCustZJson`</SwmToken> object.

```java
	@JsonProperty("INQCUSTZ")
	private InqCustZJson INQCUSTZ;
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerJson.java" line="16">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerJson.java" pos="16:1:2" line-data="	@JsonProperty(&quot;CRECUST&quot;)">`@JsonProperty`</SwmToken> annotation is used to map the JSON property <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerJson.java" pos="16:5:5" line-data="	@JsonProperty(&quot;CRECUST&quot;)">`CRECUST`</SwmToken> to the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerJson.java" pos="17:3:3" line-data="	private CrecustJson creCust;">`CrecustJson`</SwmToken> object.

```java
	@JsonProperty("CRECUST")
	private CrecustJson creCust;
```

---

</SwmSnippet>

## String Representation

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/CustomerEnquiryJson.java" pos="40:5:5" line-data="	public String toString()">`toString`</SwmToken> method in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/CustomerEnquiryJson.java" pos="42:4:4" line-data="		return &quot;CustomerEnquiryJson [INQCUSTZ=&quot; + INQCUSTZ + &quot;]&quot;;">`CustomerEnquiryJson`</SwmToken> class provides a string representation of the object, including the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/CustomerEnquiryJson.java" pos="16:5:5" line-data="	@JsonProperty(&quot;INQCUSTZ&quot;)">`INQCUSTZ`</SwmToken> field.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/CustomerEnquiryJson.java" line="39">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/CustomerEnquiryJson.java" pos="40:5:5" line-data="	public String toString()">`toString`</SwmToken> method in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/CustomerEnquiryJson.java" pos="42:4:4" line-data="		return &quot;CustomerEnquiryJson [INQCUSTZ=&quot; + INQCUSTZ + &quot;]&quot;;">`CustomerEnquiryJson`</SwmToken> class includes the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/customerenquiry/CustomerEnquiryJson.java" pos="42:7:7" line-data="		return &quot;CustomerEnquiryJson [INQCUSTZ=&quot; + INQCUSTZ + &quot;]&quot;;">`INQCUSTZ`</SwmToken> field in its string representation.

```java
	@Override
	public String toString()
	{
		return "CustomerEnquiryJson [INQCUSTZ=" + INQCUSTZ + "]";
	}
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerJson.java" line="45">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerJson.java" pos="46:5:5" line-data="	public String toString()">`toString`</SwmToken> method in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerJson.java" pos="48:4:4" line-data="		return &quot;CreateCustomerJson [CreCust=&quot; + creCust + &quot;]&quot;;">`CreateCustomerJson`</SwmToken> class includes the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/jsonclasses/createcustomer/CreateCustomerJson.java" pos="48:13:13" line-data="		return &quot;CreateCustomerJson [CreCust=&quot; + creCust + &quot;]&quot;;">`creCust`</SwmToken> field in its string representation.

```java
	@Override
	public String toString()
	{
		return "CreateCustomerJson [CreCust=" + creCust + "]";
	}
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=" repo-name="cics-banking-sample-application-cbsa-IBM-Demo-GPT"><sup>Powered by [Swimm](/)</sup></SwmMeta>
