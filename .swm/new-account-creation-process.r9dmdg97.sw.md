---
title: New Account Creation Process
---
The process of creating a new bank account involves several steps to ensure the account is properly set up and stored in the database. This document will cover:

1. Initiating the account creation
2. Handling the core account creation logic
3. Writing account details to the database
4. Ensuring database connectivity

Technical document: <SwmLink doc-title="New Account Creation Flow">[New Account Creation Flow](/.swm/new-account-creation-flow.cff6p4ru.sw.md)</SwmLink>

# [Initiating the Account Creation](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=/docs/cff6p4ru#handling-account-creation)

The account creation process begins when a request is made to create a new account. This request triggers the initiation of the account creation process. The system prepares to handle the new account by calling the internal logic responsible for the core account creation. This step ensures that the process is encapsulated and any resources are properly managed after the operation.

# [Handling the Core Account Creation Logic](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=/docs/cff6p4ru#internal-account-creation-logic)

The core logic of creating a new account involves interacting with the database to store the account details. This step checks if the account creation is successful by verifying the data provided. If the account details are valid and the database operation is successful, the system returns a confirmation response. If there is an issue, an error response is generated. This ensures that only valid accounts are created and stored.

# [Writing Account Details to the Database](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=/docs/cff6p4ru#writing-account-details-to-database)

Once the core logic confirms the account details, the next step is to write these details to the database. This involves preparing the necessary information such as the account number, balance, and customer details. The system then executes the database operation to store this information. If the operation is successful, the account is officially created and stored in the database. If there is an error, it is logged, and the process is halted to prevent incomplete data storage.

# [Ensuring Database Connectivity](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=/docs/cff6p4ru#ensuring-database-connectivity)

Throughout the account creation process, it is crucial to ensure that there is an active connection to the database. The system checks if a connection already exists and is open. If not, it attempts to establish a new connection. This step is vital to ensure that the account details can be written to the database without any connectivity issues. If the connection cannot be established, the process is aborted to prevent data inconsistencies.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=" repo-name="cics-banking-sample-application-cbsa-IBM-Demo-GPT"><sup>Powered by [Swimm](/)</sup></SwmMeta>
