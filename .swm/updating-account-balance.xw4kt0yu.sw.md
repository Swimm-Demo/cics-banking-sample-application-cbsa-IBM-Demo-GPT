---
title: Updating Account Balance
---
This document will cover the process of updating an account balance, which includes:

1. Retrieving account details
2. Establishing a database connection
3. Updating the account balance.

Technical document: <SwmLink doc-title="Updating Account Balance">[Updating Account Balance](/.swm/updating-account-balance.veauq6ho.sw.md)</SwmLink>

# [Retrieving Account Details](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=/docs/veauq6ho#retrieving-account-details)

The process begins by retrieving the account details using the account number and sort code. This step ensures that the account exists and fetches the current balance. If the account number is 99999999, it fetches the latest account details. Otherwise, it fetches the specific account details.

# [Establishing a Database Connection](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=/docs/veauq6ho#opening-a-database-connection)

Once the account details are retrieved, a connection to the database is established. This step ensures that there is an active connection to the DB2 database. If a connection already exists for the current task, it is reused. Otherwise, a new connection is created.

# [Updating the Account Balance](https://app.swimm.io/repos/Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=/docs/veauq6ho#handling-account-balance-updates)

After establishing a database connection, the specified amount is added to the current balance. The new balance is then updated in the database. This step ensures that the account balance reflects the latest transaction, whether it is a debit or a credit.

&nbsp;

*This is an auto-generated document by Swimm 🌊 and has not yet been verified by a human*

<SwmMeta version="3.0.0" repo-id="Z2l0aHViJTNBJTNBY2ljcy1iYW5raW5nLXNhbXBsZS1hcHBsaWNhdGlvbi1jYnNhLUlCTS1EZW1vLUdQVCUzQSUzQVN3aW1tLURlbW8=" repo-name="cics-banking-sample-application-cbsa-IBM-Demo-GPT"><sup>Powered by [Swimm](/)</sup></SwmMeta>
