---
title: Exploring Customer Services Controllers
---
# Exploring Customer Services Controllers

Controllers in the Customer Services API are responsible for handling HTTP requests and mapping them to the appropriate service methods. They manage the flow of data between the user interface and the backend services, ensuring that user inputs are processed correctly and responses are returned.

In the Customer Services API, controllers are implemented using Spring Boot and are annotated with <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="53:0:1" line-data="@Controller">`@Controller`</SwmToken> to indicate their role in handling web requests.

They define various endpoints for different customer service operations such as account inquiries, customer inquiries, account creation, customer creation, account updates, customer updates, account deletions, and customer deletions.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="53">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class is annotated with <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="53:0:1" line-data="@Controller">`@Controller`</SwmToken> and implements <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:8:8" line-data="public class WebController implements WebMvcConfigurer">`WebMvcConfigurer`</SwmToken>, indicating its role in handling web requests.

```java
@Controller
public class WebController implements WebMvcConfigurer
{
```

---

</SwmSnippet>

Each endpoint is mapped to a specific URL and HTTP method (GET or POST), and the corresponding method in the controller processes the request and returns the appropriate view or response.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="106">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="108:5:5" line-data="	public String showCustServices(Model model)">`showCustServices`</SwmToken> method is mapped to the GET request at the root endpoint and returns the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="112:4:4" line-data="		return &quot;customerServices&quot;;">`customerServices`</SwmToken> view.

```java
	@GetMapping(value =
	{ "","/services", "/" })
	public String showCustServices(Model model)
	{

		model.addAttribute("contextPath", "");
		return "customerServices";
	}
```

---

</SwmSnippet>

## Customer Services API Endpoints

The Customer Services API defines several endpoints to handle various customer service operations.

### <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="125:5:5" line-data="	public String showAcctForm(AccountEnquiryForm accountEnquiryForm)">`showAcctForm`</SwmToken>

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="125:5:5" line-data="	public String showAcctForm(AccountEnquiryForm accountEnquiryForm)">`showAcctForm`</SwmToken> method is mapped to the GET request at the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="124:5:6" line-data="	@GetMapping(&quot;/enqacct&quot;)">`/enqacct`</SwmToken> endpoint. It displays the account enquiry form to the user.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="124">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="125:5:5" line-data="	public String showAcctForm(AccountEnquiryForm accountEnquiryForm)">`showAcctForm`</SwmToken> method definition in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class.

```java
	@GetMapping("/enqacct")
	public String showAcctForm(AccountEnquiryForm accountEnquiryForm)
	{
```

---

</SwmSnippet>

### <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="138:5:5" line-data="	public String returnAcct(@Valid AccountEnquiryForm accountEnquiryForm,">`returnAcct`</SwmToken>

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="138:5:5" line-data="	public String returnAcct(@Valid AccountEnquiryForm accountEnquiryForm,">`returnAcct`</SwmToken> method is mapped to the POST request at the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="124:5:6" line-data="	@GetMapping(&quot;/enqacct&quot;)">`/enqacct`</SwmToken> endpoint. It processes the submitted account enquiry form and returns the account details.

<SwmSnippet path="/src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" line="137">

---

The <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="138:5:5" line-data="	public String returnAcct(@Valid AccountEnquiryForm accountEnquiryForm,">`returnAcct`</SwmToken> method definition in the <SwmToken path="src/Z-OS-Connect-Customer-Services-Interface/src/main/java/com/ibm/cics/cip/bank/springboot/customerservices/controllers/WebController.java" pos="54:4:4" line-data="public class WebController implements WebMvcConfigurer">`WebController`</SwmToken> class.

```java
	@PostMapping("/enqacct")
	public String returnAcct(@Valid AccountEnquiryForm accountEnquiryForm,
			BindingResult bindingResult, Model model)
			throws JsonProcessingException
	{
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=" repo-name="cics-banking-sample-application-cbsa-IBM-Demo-GPT"><sup>Powered by [Swimm](/)</sup></SwmMeta>
