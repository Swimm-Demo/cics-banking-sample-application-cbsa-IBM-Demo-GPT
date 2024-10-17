---
title: Customer Deletion Process
---
The process of deleting a customer and their associated accounts involves several steps to ensure that all related data is correctly removed. This document will cover:

1. Initiating the deletion process
2. Deleting associated accounts
3. Removing the customer record
4. Logging the transaction

Technical document: <SwmLink doc-title="Customer Deletion Flow">[Customer Deletion Flow](/.swm/customer-deletion-flow.2ko00w8z.sw.md)</SwmLink>

# [Initiating the deletion process](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=/docs/2ko00w8z#deletefromdb)

The deletion process begins by calling an external API to delete the customer. This API call is crucial as it ensures that the request to delete the customer is properly communicated to the system. The system then prepares to handle the response to ensure that the customer data is correctly parsed and updated.

# [Deleting associated accounts](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=/docs/2ko00w8z#deleting-associated-accounts)

Before the customer record can be deleted, all accounts associated with the customer must be identified and deleted. The system retrieves all accounts linked to the customer and iterates through them. Each account is then individually verified and deleted. This step ensures that no orphaned accounts remain in the system, which could lead to data inconsistencies.

# [Removing the customer record](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=/docs/2ko00w8z#deletecustomerinternal)

Once all associated accounts have been successfully deleted, the system proceeds to delete the customer record. This involves removing the customer's data from the database. The system ensures that the customer record is completely removed to maintain data integrity and prevent any future references to the deleted customer.

# [Logging the transaction](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=/docs/2ko00w8z#deleteaccountinternal)

After the customer and all associated accounts have been deleted, the system logs the transaction. This logging is essential for maintaining an audit trail, which can be used for future reference or troubleshooting. The log includes details of the deletion process, ensuring transparency and accountability.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=" repo-name="cics-banking-sample-application-cbsa-IBM-Demo-GPT"><sup>Powered by [Swimm](/)</sup></SwmMeta>
