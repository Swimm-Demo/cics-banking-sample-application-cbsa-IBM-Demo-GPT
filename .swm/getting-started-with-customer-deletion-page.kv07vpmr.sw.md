---
title: Getting Started with Customer Deletion Page
---
## Overview

The Customer Deletion Page is a part of the Frontend Application that allows users to remove a customer from the system. To delete a customer, the user must enter the customer number and submit the request. The system then retrieves the customer details and displays them. If the customer is found, the page will show the customer details along with their associated accounts. The user can then confirm the deletion. Upon successful deletion, a confirmation message is displayed. Note that deleting a customer also removes all associated accounts. If no customer is found with the entered number, a modal will appear informing the user that no customers were found.

## Accessing the Customer Delete Page

To remove a customer, click on 'Delete customer' from the landing page.

## Handling Customer Number Input

The function <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="33:3:3" line-data="  function handleCustomerNumberInput(e){">`handleCustomerNumberInput`</SwmToken> captures the customer number entered by the user.

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" line="33">

---

The function <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="33:3:3" line-data="  function handleCustomerNumberInput(e){">`handleCustomerNumberInput`</SwmToken> captures the customer number entered by the user and updates the state with this value.

```javascript
  function handleCustomerNumberInput(e){
    setSearchCustomerValue(e.target.value)
  }
```

---

</SwmSnippet>

## Submitting the Delete Request

The function <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="45:5:5" line-data="  async function handleSubmitButtonClick(){">`handleSubmitButtonClick`</SwmToken> submits the delete request and calls <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="47:3:3" line-data="    await getCustomerByNum(searchQuery)">`getCustomerByNum`</SwmToken> to retrieve customer details.

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" line="45">

---

The function <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="45:5:5" line-data="  async function handleSubmitButtonClick(){">`handleSubmitButtonClick`</SwmToken> is triggered when the user submits the customer number. It calls <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="47:3:3" line-data="    await getCustomerByNum(searchQuery)">`getCustomerByNum`</SwmToken> to retrieve customer details and then displays the results.

```javascript
  async function handleSubmitButtonClick(){
    let searchQuery = searchCustomerValue;
    await getCustomerByNum(searchQuery)
    .then(display())
  }
```

---

</SwmSnippet>

## Retrieving Customer Details

The function <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="47:3:3" line-data="    await getCustomerByNum(searchQuery)">`getCustomerByNum`</SwmToken> retrieves the customer details using the entered customer number.

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" line="63">

---

The function <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="67:5:5" line-data="   async function getCustomerByNum(searchQuery) {">`getCustomerByNum`</SwmToken> retrieves customer details based on the customer number entered by the user. It sets the state with the retrieved customer details and associated accounts.

```javascript
   /**
    * Finds the customer using the customerNumber entered by the user and creates an array from the server response
    * customerDetailsRows' state is set to this array
    */
   async function getCustomerByNum(searchQuery) {
     let responseData;
     let rowBuild = [];
     await axios
       .get(process.env.REACT_APP_CUSTOMER_URL + `/${searchQuery}`)
       .then(response => {
         responseData = response.data;
         try {
           let row;
           let formattedDOB = getDay(responseData.dateOfBirth) + "-" + getMonth(responseData.dateOfBirth) + "-" + getYear(responseData.dateOfBirth)
           let formattedReviewDate = getDay(responseData.customerCreditScoreReviewDate) + "-" + getMonth(responseData.customerCreditScoreReviewDate) +
            "-" + getYear(responseData.customerCreditScoreReviewDate)
           row = {
             id: parseInt(responseData.id).toString(),
             customerNumber: parseInt(responseData.id).toString(),
             sortCode: responseData.sortCode,
             customerName: responseData.customerName,
```

---

</SwmSnippet>

## Displaying No Results Modal

The function <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="41:3:3" line-data="  function displayNoResultsModal(){">`displayNoResultsModal`</SwmToken> toggles the modal that informs the user if no customer is found.

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" line="41">

---

The function <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="41:3:3" line-data="  function displayNoResultsModal(){">`displayNoResultsModal`</SwmToken> toggles the modal that informs the user if no customer is found.

```javascript
  function displayNoResultsModal(){
    setIsNoResultsModalOpen(wasOpened => !wasOpened)
```

---

</SwmSnippet>

## Main Functions

There are several main functions in this folder. Some of them are <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="22:2:2" line-data="const CustomerDeletePage = () =&gt; {">`CustomerDeletePage`</SwmToken>, <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="45:5:5" line-data="  async function handleSubmitButtonClick(){">`handleSubmitButtonClick`</SwmToken>, <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="47:3:3" line-data="    await getCustomerByNum(searchQuery)">`getCustomerByNum`</SwmToken>, and `deleteCustomer`. We will dive a little into <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="45:5:5" line-data="  async function handleSubmitButtonClick(){">`handleSubmitButtonClick`</SwmToken> and `deleteCustomer`.

### <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="22:2:2" line-data="const CustomerDeletePage = () =&gt; {">`CustomerDeletePage`</SwmToken>

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="22:2:2" line-data="const CustomerDeletePage = () =&gt; {">`CustomerDeletePage`</SwmToken> function is the main component for the customer deletion page. It handles the state and user interactions for deleting a customer.

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" line="22">

---

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="22:2:2" line-data="const CustomerDeletePage = () =&gt; {">`CustomerDeletePage`</SwmToken> function is the main component for the customer deletion page. It handles the state and user interactions for deleting a customer.

```javascript
const CustomerDeletePage = () => {

  /**
   * States to store isOpened value of the table and the value the user has entered to search with
   */
  const [isOpened, setIsOpened] = useState(false);
  var [searchCustomerValue, setSearchCustomerValue] = useState("")
  const [customerDetailsRows, setRows] = useState([]);
  const [accountDetailsRows, setAccountRows] = useState([]);
  const [isNoResultsModalOpen, setIsNoResultsModalOpen] = useState(false)

  function handleCustomerNumberInput(e){
    setSearchCustomerValue(e.target.value)
  }

  function display() {
    setIsOpened(wasOpened => !wasOpened);
  }

  function displayNoResultsModal(){
    setIsNoResultsModalOpen(wasOpened => !wasOpened)
```

---

</SwmSnippet>

### <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="47:3:3" line-data="    await getCustomerByNum(searchQuery)">`getCustomerByNum`</SwmToken>

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="47:3:3" line-data="    await getCustomerByNum(searchQuery)">`getCustomerByNum`</SwmToken> function retrieves customer details based on the customer number entered by the user. It sets the state with the retrieved customer details and associated accounts.

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" line="63">

---

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDeletePage/CustomerDeletePage.js" pos="67:5:5" line-data="   async function getCustomerByNum(searchQuery) {">`getCustomerByNum`</SwmToken> function retrieves customer details based on the customer number entered by the user. It sets the state with the retrieved customer details and associated accounts.

```javascript
   /**
    * Finds the customer using the customerNumber entered by the user and creates an array from the server response
    * customerDetailsRows' state is set to this array
    */
   async function getCustomerByNum(searchQuery) {
     let responseData;
     let rowBuild = [];
     await axios
       .get(process.env.REACT_APP_CUSTOMER_URL + `/${searchQuery}`)
       .then(response => {
         responseData = response.data;
         try {
           let row;
           let formattedDOB = getDay(responseData.dateOfBirth) + "-" + getMonth(responseData.dateOfBirth) + "-" + getYear(responseData.dateOfBirth)
           let formattedReviewDate = getDay(responseData.customerCreditScoreReviewDate) + "-" + getMonth(responseData.customerCreditScoreReviewDate) +
            "-" + getYear(responseData.customerCreditScoreReviewDate)
           row = {
             id: parseInt(responseData.id).toString(),
             customerNumber: parseInt(responseData.id).toString(),
             sortCode: responseData.sortCode,
             customerName: responseData.customerName,
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=" repo-name="cics-banking-sample-application-cbsa-IBM-Demo-GPT"><sup>Powered by [Swimm](/)</sup></SwmMeta>
