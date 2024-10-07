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

### Page navigation

Most of the GET APIs in <SwmToken path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> are used for page navigation by returning the template name for the requested page.\
For example the <SwmToken path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="125:5:5" line-data="	public String showAcctForm(AccountEnquiryForm accountEnquiryForm)">`showAcctForm`</SwmToken> function returns <SwmToken path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="129:3:3" line-data="		return ACCOUNT_ENQUIRY_FORM;">`ACCOUNT_ENQUIRY_FORM`</SwmToken>.

```java
	// Get request for when first navigating to the page
	@GetMapping("/enqacct")
	public String showAcctForm(AccountEnquiryForm accountEnquiryForm)
	{
		// String relates to the page template found in
		// /src/main/resources/templates
		return ACCOUNT_ENQUIRY_FORM;
	}
```

---

</SwmSnippet>

<SwmSnippet path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="137">

---

## Enquire account

The enquire account endpoint is defined in the <SwmToken path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="138:5:5" line-data="	public String returnAcct(@Valid AccountEnquiryForm accountEnquiryForm,">`returnAcct`</SwmToken> method.

```
	@PostMapping("/enqacct")
	public String returnAcct(@Valid AccountEnquiryForm accountEnquiryForm,
			BindingResult bindingResult, Model model)
			throws JsonProcessingException
	{
```

---

</SwmSnippet>

<SwmSnippet path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="241">

---

## Enquire customer

The enquire customer endpoint is defined in the <SwmToken path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="242:5:5" line-data="	public String returnCust(@Valid CustomerEnquiryForm customerEnquiryForm,">`returnCust`</SwmToken> method.

```
	@PostMapping("/enqcust")
	public String returnCust(@Valid CustomerEnquiryForm customerEnquiryForm,
			BindingResult bindingResult, Model model)
			throws JsonProcessingException
	{
```

---

</SwmSnippet>

<SwmSnippet path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="310">

---

## List all accounts belonging to a customer

The list all accounts belonging to a customer endpoint is defined in the <SwmToken path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="311:5:5" line-data="	public String returnListAcc(@Valid CustomerEnquiryForm customerEnquiryForm,">`returnListAcc`</SwmToken> method.

```
	@PostMapping("/listacc")
	public String returnListAcc(@Valid CustomerEnquiryForm customerEnquiryForm,
			BindingResult bindingResult, Model model)
			throws JsonProcessingException
	{
```

---

</SwmSnippet>

<SwmSnippet path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="381">

---

## Create an account

The create an account endpoint is defined in the <SwmToken path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="382:5:5" line-data="	public String processCreateAcc(@Valid CreateAccountForm createAccForm,">`processCreateAcc`</SwmToken> method.

```
	@PostMapping("/createacc")
	public String processCreateAcc(@Valid CreateAccountForm createAccForm,
			BindingResult bindingResult, Model model)
			throws JsonProcessingException
	{
```

---

</SwmSnippet>

<SwmSnippet path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="490">

---

## Create a customer

The create a customer endpoint is defined in the <SwmToken path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="491:5:5" line-data="	public String processCreateCust(@Valid CreateCustomerForm createCustForm,">`processCreateCust`</SwmToken> method.

```
	@PostMapping("/createcust")
	public String processCreateCust(@Valid CreateCustomerForm createCustForm,
			BindingResult bindingResult, Model model)
			throws JsonProcessingException
	{
```

---

</SwmSnippet>

<SwmSnippet path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="587">

---

## Update an account

The update an account endpoint is defined in the <SwmToken path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="588:5:5" line-data="	public String processCreateAcc(@Valid UpdateAccountForm updateAccountForm,">`processCreateAcc`</SwmToken> method.

```
	@PostMapping("/updateacc")
	public String processCreateAcc(@Valid UpdateAccountForm updateAccountForm,
			BindingResult bindingResult, Model model)
			throws JsonProcessingException
	{
```

---

</SwmSnippet>

<SwmSnippet path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="688">

---

## Update a customer

The update a customer endpoint is defined in the <SwmToken path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="689:5:5" line-data="	public String processUpdateCust(">`processUpdateCust`</SwmToken> method.

```
	@PostMapping("/updatecust")
	public String processUpdateCust(
			@Valid UpdateCustomerForm updateCustomerForm,
			BindingResult bindingResult, Model model)
			throws JsonProcessingException
	{
```

---

</SwmSnippet>

<SwmSnippet path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="791">

---

## Delete an account

The delete an account endpoint is defined in the <SwmToken path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="792:5:5" line-data="	public String deleteAcct(@Valid AccountEnquiryForm accountEnquiryForm,">`deleteAcct`</SwmToken> method.

```
	@PostMapping("/delacct")
	public String deleteAcct(@Valid AccountEnquiryForm accountEnquiryForm,
			BindingResult bindingResult, Model model)
			throws JsonProcessingException
	{
```

---

</SwmSnippet>

<SwmSnippet path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="856">

---

## Delete a customer

The delete a customer endpoint is defined in the <SwmToken path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="857:5:5" line-data="	public String deleteCust(@Valid CustomerEnquiryForm customerEnquiryForm,">`deleteCust`</SwmToken> method.

```
	@PostMapping("/delcust")
	public String deleteCust(@Valid CustomerEnquiryForm customerEnquiryForm,
			BindingResult bindingResult, Model model)
			throws JsonProcessingException
	{
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm AI 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa"><sup>Powered by [Swimm](https://staging.swimm.cloud/)</sup></SwmMeta>
