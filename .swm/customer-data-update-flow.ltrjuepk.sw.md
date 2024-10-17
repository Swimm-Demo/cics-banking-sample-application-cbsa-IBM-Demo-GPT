---
title: Customer Data Update Flow
---
This document will cover the Customer Data Update Flow, which includes:

1. Initiating the customer data update
2. Handling external update requests
3. Performing internal validation and updates
4. Updating the customer record in the database.

Technical document: <SwmLink doc-title="Customer Data Update Flow">[Customer Data Update Flow](/.swm/customer-data-update-flow.yvvjw1f8.sw.md)</SwmLink>

# [Initiating the customer data update](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=/docs/yvvjw1f8#handling-customer-data-updates)

The process begins when a customer data update is initiated. This involves creating a `CustomerResource` and a `CustomerJSON` object, which are used to set the customer details. The system then calls the `updateCustomerExternal` method to handle the external update request. If the response status is 200, it means the update was successful, and the customer object is updated with the new data.

# [Handling external update requests](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=/docs/yvvjw1f8#external-customer-update)

The `updateCustomerExternal` method is responsible for handling external update requests. It calls the `updateCustomerInternal` method to perform the actual update. After the internal update is completed, the `HBankDataAccess` session is terminated, and the response is returned. This step ensures that the external request is properly managed and that the session is closed after the update.

# [Performing internal validation and updates](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=/docs/yvvjw1f8#internal-customer-update-and-validation)

The `updateCustomerInternal` method performs detailed validation of the customer data. It checks for null values, validates the customer title and sort code, and updates the customer record in the VSAM database. If any validation fails, an appropriate error response is returned. This step ensures that the customer data is accurate and meets the required standards before being updated in the database.

# [Updating the customer record in the database](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=/docs/yvvjw1f8#updating-customer-record-in-vsam)

The final step involves updating the customer record in the VSAM database. The `updateCustomer` method reads the existing record, updates the necessary fields, and rewrites the record. If the record is not found, a `Customer` object with a `notFound` flag set to true is returned. This step ensures that the customer record is accurately updated in the database, reflecting the latest information.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=" repo-name="cics-banking-sample-application-cbsa-IBM-Demo-GPT"><sup>Powered by [Swimm](/)</sup></SwmMeta>
