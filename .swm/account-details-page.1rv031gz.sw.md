---
title: Account details page
---
The Account details page is a React component that displays account information and allows the user to update the account details. It uses the Carbon Design System to create a user-friendly interface. The page fetches account data from an API and displays it in a table. The user can edit the account details and save the changes.

<SwmSnippet path="src/bank-application-frontend/src/App.js" line="76">

---

## Where is the Account details page used?

The Account details page is used in the <SwmPath>[src/bank-application-frontend/src/App.js](/src/bank-application-frontend/src/App.js)</SwmPath> file.

```
            <Route
              path="/Admin/account_details"
              component={AccountDetailsPage}
            />
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" line="44">

---

## How does the Account details page work?

The Account details page works by fetching account data from an API using the <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" pos="47:1:1" line-data="        getCustomerAccounts(searchQuery)">`getCustomerAccounts`</SwmToken> function. The user can then view the account details in a table and make changes to the account information. The changes are saved using the <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" pos="44:3:3" line-data="  function handleClick() {">`handleClick`</SwmToken> function, which makes a request to the API to get the account details.

```javascript
  function handleClick() {
    let searchQuery = userInput;
    if (userInput.length !== 0){
        getCustomerAccounts(searchQuery)
        setIsOpened(wasOpened => !wasOpened)
      } else {
        displayNoResults()
      }
  }
```

---

</SwmSnippet>

<SwmSnippet path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" line="156">

---

## What are the main components of the Account details page?

The main components of the Account details page are the <SwmToken path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" pos="9:2:2" line-data="import AccountDetailsTable from &#39;./AccountDetailsTable&#39;;">`AccountDetailsTable`</SwmToken> component, which displays the account details in a table, and the UpdateAccountModal component, which allows the user to update the account details.

```
                {isOpened && (
                  <Column lg={16}>
                    <AccountDetailsTable accountMainRow={accountMainRow}/>
                  </Column>
                )}
```

---

</SwmSnippet>

# Main functions

There are several functions in the Account details page that allow the user to view and interact with the account details.

<SwmSnippet path="src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" line="70">

---

## Display customer accounts

This function fetch and display the customer's accounts.

```
  async function getCustomerAccounts(searchQuery) {
    let account;
    let rowBuild = [];
    await axios
      .get(process.env.REACT_APP_ACCOUNT_URL + `/${searchQuery}`)
      .then(response => {
        account = response.data;
      }).catch (function (error) {
        if (error.response){
```

---

</SwmSnippet>

<SwmSnippet path="/src/bank-application-frontend/src/content/AccountDetailsPage/AccountDetailsPage.js" line="44">

---

## Search account details

This function handles submit click with the account details.

```javascript
  function handleClick() {
    let searchQuery = userInput;
    if (userInput.length !== 0){
        getCustomerAccounts(searchQuery)
        setIsOpened(wasOpened => !wasOpened)
      } else {
        displayNoResults()
      }
  }
```

---

</SwmSnippet>

&nbsp;

*This is an auto-generated document by Swimm AI 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vJTNBJTNBU3dpbW0tRGVtbw==" repo-name="cics-banking-sample-application-cbsa"><sup>Powered by [Swimm](https://staging.swimm.cloud/)</sup></SwmMeta>
