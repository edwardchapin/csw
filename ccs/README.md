CCS - Command and Control Service
=================================

The ccs project defines the basic classes and traits for the *Command and Control Service*.

Related projects are the *util* project, which defines the types for *configurations* and the
*kvs* project, which defines a key/value store, which can be used to set/get state variables.

This is an actor based project.
The main actors (or actor traits) here are:

- *AssemblyController* - trait for an actor that accepts configurations for an Assembly and communicates with
    one or more HCDs or other assemblies before replying with a command status

- *HcdController* - trait for an actor that accepts configurations (from an Assembly) for an HCD.
    The HCD controller sets a state variable, which is used by the assembly to determine when a command has completed.

- *PeriodicHcdController* - like HcdController, but has a Queue of configurations that it checks at a specified rate.
    This controller wakes up regularly, checks the incoming queue and updates its state variable with the current
    state of the HCD.

- *StateMatcherActor* - an actor used to match demand and current state and then notify a given actor


