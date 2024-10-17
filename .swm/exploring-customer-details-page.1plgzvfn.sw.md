---
title: Exploring Customer Details Page
---
# Overview

The Customer Details Page is a component that allows users to view and manage customer information. It provides functionalities to search for customers by their number or name, displaying the results in a table format. The page includes input fields for entering the customer number or name, and buttons to submit the search query.

# Viewing Customer Details

To see customer details, click the 'View customer details' button on the landing page. When a search is performed, the page fetches customer data from the backend and populates the table with the retrieved information. The table displays various customer details such as customer number, sort code, name, address, date of birth, credit score, and next review date.

# Updating Customer Details

To amend customer details, click the 'Update customer details' button on the landing page.

# Enquiring on an Account

To enquire on an account, click on 'View account details' from the landing page. The page can fetch and display account details associated with a customer, including account number, sort code, account type, interest rate, overdraft limit, available balance, actual balance, account opened date, and last statement date.

# Listing Customer Accounts

To display the accounts for a particular customer, click on 'List accounts belonging to customer'.

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" line="23">

---

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="23:2:2" line-data="const CustomerDetailsPage = () =&gt; {">`CustomerDetailsPage`</SwmToken> function initializes state variables for managing the visibility of the table and modals, as well as storing customer and account details.

```javascript
const CustomerDetailsPage = () => {
  /**
   * States for table visibility and entered search values from the user
   */
  const [isOpened, setTableOpened] = useState(false);
  const [customerDetailsRows, setRows] = useState([]);
  const [accountDetailsRows, setAccountRows] = useState([]);
  const [noResultsOpened, setNoResultsOpened] = useState(false)
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" line="217">

---

Input fields for entering customer number or name, with event handlers to update the state based on user input.

```javascript
                      className="customer-list-view"
                      id="customerNum"
                      label="Enter a customer's number to view details"
                      placeholder="e.g 1000"
                      invalidText='Please provide a valid number'
                      onChange={e => handleNumInputChange(e)}
                      hideSteppers
                      allowEmpty
                    />
                    <div style={{ marginTop: '20px' }}>
                      <TextInput
                        className="customer-list-name"
                        id="customerNameInput"
                        type="text"
                        labelText="Alternatively, enter the customer's name:"
                        placeholder="Case sensitive"
                        onChange={e => handleNameInputChange(e)}
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" line="116">

---

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="119:5:5" line-data="  async function getCustomerByNum(searchQuery) {">`getCustomerByNum`</SwmToken> function fetches customer details by customer number and updates the state with the retrieved data.

```javascript
  /**
   * Gets the customer from a given customerNum, builds an array from the response and sets customerDetailsRows' state to this array
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
            customerAddress: responseData.customerAddress,
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" line="72">

---

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="75:5:5" line-data="  async function getCustomersByName(searchQuery) {">`getCustomersByName`</SwmToken> function fetches customer details by name and updates the state with the retrieved data.

```javascript
  /**
   * Gets the first 10 customers from a given name, builds an array from the response and sets customerDetailsRows' state to this array
   */
  async function getCustomersByName(searchQuery) {
    let responseData;
    let rowBuild = [];
    await axios
      .get(process.env.REACT_APP_CUSTOMER_URL + `/name?name=${searchQuery}&limit=10`)
      .then(response => {
        responseData = response.data;
        try {
          responseData.customers.forEach(customer => {
            let formattedDOB = getDay(customer.dateOfBirth) + "-" + getMonth(customer.dateOfBirth) + "-" + getYear(customer.dateOfBirth)
            let formattedReviewDate = getDay(customer.customerCreditScoreReviewDate) + "-" + getMonth(customer.customerCreditScoreReviewDate) +
            "-" + getYear(customer.customerCreditScoreReviewDate)
            let row;
            row = {
              id: parseInt(customer.id).toString(),
              customerNumber: parseInt(customer.id).toString(),
              sortCode: customer.sortCode,
              customerName: customer.customerName,
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" line="158">

---

The <SwmToken path="src/bank-application-frontend/src/content/CustomerDetailsPage/CustomerDetailsPage.js" pos="161:5:5" line-data="  async function getAccountsForCustomers(customerID) {">`getAccountsForCustomers`</SwmToken> function fetches account details for a given customer ID and updates the state with the retrieved data.

```javascript
  /**
   * Gets the accounts for a given customerID, builds an array from the response and sets accountDetailsRows' state to this array
   */
  async function getAccountsForCustomers(customerID) {
    let accountData;
    let accountRowBuild = []
    await axios
      .get(process.env.REACT_APP_ACCOUNT_URL + `/retrieveByCustomerNumber/${customerID}`)
      .then(response => {
        accountData = response.data;
        let row;
        accountData.accounts.forEach(account => {
          row = {
            accountNumber: account.id,
            sortCode: account.sortCode,
            accountType: account.accountType,
            interestRate: account.interestRate,
            overdraft: account.overdraft,
            availableBalance: account.availableBalance,
            actualBalance: account.actualBalance,
            accountOpened: account.dateOpened,
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=" repo-name="cics-banking-sample-application-cbsa-IBM-Demo-GPT"><sup>Powered by [Swimm](/)</sup></SwmMeta>
