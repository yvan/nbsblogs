*> call verbsubroutine is a program that is compiled
*> independently but cannot be run independently
*> there are external/interanl subroutines
*>call: this verb is used to trasnfer CONTROL
*> to the program being called. called program needs:
*> linkage section, containing what data elements are passed to the
*> program. procedure division containing a list of
*> variables from the calling program.
*> an exist statement to transfer control back to caller.

*> by default cobol is call by reference (all variables in
*> calling program get cahnged in the called program and
*> stay cahnged after called program returns control).
*> you can specify BY command to pass by content (the value).

*> static vs dynamic call
*> static - occcurs when a program is compiled with 'nodynam'
*> static called program is loaded in storage at compile time

*> dynamic - occurs when a program is compiled with 'dynam' and
*> nodll compiler option. a dynamic called program is loaded
*> into storage(ram) at runtime.
