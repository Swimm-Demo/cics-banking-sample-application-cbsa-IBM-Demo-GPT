---
title: Customer Services Overview
---
<SwmSnippet path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="53">

---

Customer Services is a Spring Boot application that provides services to reduce people's queueing time.

```
@Controller
public class WebController implements WebMvcConfigurer
{
```

---

</SwmSnippet>

# Running and Debugging

The project is written in Java with Spring Boot, with Thymeleaf templates powering the web pages.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/pom.xml" line="1">

---

The project is built using Maven and has dependencies on various libraries and frameworks.

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!-- Copyright IBM Corp. 2023 -->
<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
	xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 https://maven.apache.org/xsd/maven-4.0.0.xsd">
	<modelVersion>4.0.0</modelVersion>
	<parent>
		<groupId>org.springframework.boot</groupId>
		<artifactId>spring-boot-starter-parent</artifactId>
		<version>3.2.5</version>
		<relativePath /> <!-- lookup parent from repository -->
```

---

</SwmSnippet>

The project can be run using the command `mvn spring-boot:run`.

The project can be debugged using a Java debugger, such as Visual Studio Code or Eclipse.

# APIs

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="106">

---

## User APIs

The user APIs are defined in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class.

```java
	@GetMapping(value =
	{ "","/services", "/" })
	public String showCustServices(Model model)
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="123">

---

## Enquire account

The enquire account endpoint is defined in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="125:5:5" line-data="	public String showAcctForm(AccountEnquiryForm accountEnquiryForm)">`showAcctForm`</SwmToken> method.

```java
	// Get request for when first navigating to the page
	@GetMapping("/enqacct")
	public String showAcctForm(AccountEnquiryForm accountEnquiryForm)
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="234">

---

## Enquire customer

The enquire customer endpoint is defined in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="235:5:5" line-data="	public String showCustForm(CustomerEnquiryForm customerEnquiryForm)">`showCustForm`</SwmToken> method.

```java
	@GetMapping("/enqcust")
	public String showCustForm(CustomerEnquiryForm customerEnquiryForm)
	{
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="301">

---

## List all accounts belonging to a customer

The list all accounts belonging to a customer endpoint is defined in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="304:5:5" line-data="	public String showListAccForm(CustomerEnquiryForm customerEnquiryForm)">`showListAccForm`</SwmToken> method.

```java
	// Similar form to enqCust since we're still only asking for a customer
	// number
	@GetMapping("/listacc")
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="371">

---

## Create an account

The create an account endpoint is defined in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="373:5:5" line-data="	public String showCreateAccForm(CreateAccountForm createAccForm,">`showCreateAccForm`</SwmToken> method.

```java
	// 4. Create an account
	@GetMapping("/createacc")
	public String showCreateAccForm(CreateAccountForm createAccForm,
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="482">

---

## Create a customer

The create a customer endpoint is defined in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="483:5:5" line-data="	public String showCreateCustForm(CreateCustomerForm createCustForm,">`showCreateCustForm`</SwmToken> method.

```java
	@GetMapping("/createcust")
	public String showCreateCustForm(CreateCustomerForm createCustForm,
			Model model)
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="576">

---

## Update an account

The update an account endpoint is defined in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="577:5:5" line-data="	public String showUpdateAccountForm(UpdateAccountForm updateAccForm,">`showUpdateAccountForm`</SwmToken> method.

```java
	@GetMapping("/updateacc")
	public String showUpdateAccountForm(UpdateAccountForm updateAccForm,
			Model model)
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="679">

---

## Update a customer

The update a customer endpoint is defined in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="680:5:5" line-data="	public String showUpdateAccountForm(UpdateCustomerForm updateCustomerForm,">`showUpdateAccountForm`</SwmToken> method.

```java
	@GetMapping("/updatecust")
	public String showUpdateAccountForm(UpdateCustomerForm updateCustomerForm,
			Model model)
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="784">

---

## Delete an account

The delete an account endpoint is defined in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="785:5:5" line-data="	public String showDelAcctForm(AccountEnquiryForm accountEnquiryForm)">`showDelAcctForm`</SwmToken> method.

```java
	@GetMapping("/delacct")
	public String showDelAcctForm(AccountEnquiryForm accountEnquiryForm)
	{
```

---

</SwmSnippet>

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="849">

---

## Delete a customer

The delete a customer endpoint is defined in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="850:5:5" line-data="	public String showDelCustForm(CustomerEnquiryForm customerEnquiryForm)">`showDelCustForm`</SwmToken> method.

```java
	@GetMapping("/delcust")
	public String showDelCustForm(CustomerEnquiryForm customerEnquiryForm)
	{
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm AI 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa"><sup>Powered by [Swimm](https://staging.swimm.cloud/)</sup></SwmMeta>
