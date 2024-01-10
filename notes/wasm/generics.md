.chat how do I do generic programming in web assembly? that is to say, how would I express functions that use parametric polymorphism?
[11:40 AM]
BOT
 Snek: WebAssembly itself, as a low-level assembly-like language, does not natively support higher-level concepts like generics or parametric polymorphism directly in the way that languages such as Java or C# might. However, you can simulate generic programming concepts within your WebAssembly modules by using a few different strategies, depending on your source language and the tools you're using to compile to WebAssembly.

If you are using a language that supports generics and targets WebAssembly, such as Rust or AssemblyScript, you can employ their native mechanisms for generics, and the respective compilers will handle the translation to WebAssembly appropriately. For example:

Rust: When you write Rust code that leverages generics, the Rust compiler (rustc), in conjunction with the wasm-bindgen tool, will produce WebAssembly code that works as expected for your generic functions. Rust achieves this through monomorphization, where the compiler generates concrete instances of your generic types/functions for each type they're used with.

AssemblyScript: Which is a variant of TypeScript that targets WebAssembly, allows for generic types and functions. The AssemblyScript compiler generates specific WebAssembly code for each instantiation of a generic type or function.

In cases where your source language doesn't natively support generics or you're writing WebAssembly directly with the text format (.wat) or binary format (.wasm), you will have to emulate generic behavior. Here are a few general strategies:

Manual Duplication: Manually create multiple versions of the same function for different types. This can be error-prone and lead to code duplication.

Type Erasure: Instead of having type-specific logic, use a common representation (like bytes) and interpret the data appropriately within your functions.
[11:40 AM]
BOT
 Snek:
Custom Abstractions: Building custom abstractions or using dynamic typing where you check the type at runtime before performing the necessary operations.

Remember that each of these approaches will have implications on your final WebAssembly module's performance, size, and complexity. It's important to choose the strategy that best fits your specific use case and constraints.

Also, keep in mind that the WebAssembly ecosystem is constantly evolving, with proposals like the WebAssembly Interface Types, which aims to define a new shared type system across different languages and environments, which might eventually offer better support for generics in WebAssembly.
[11:44 AM]lowercase J: .chat say more about dynamic types in the context of web assembly?
[11:44 AM]
BOT
 Snek: Certainly, dynamic typing in the context of WebAssembly requires different strategies from those used in dynamic languages like JavaScript or Python. Since WebAssembly is statically typed and operates on a few primitive types, dynamic behavior must be implemented manually at a higher level.

Here’s how you could approach dynamic typing with WebAssembly:

Uniform Representation: Choose a uniform representation for all values. For instance, you could represent everything as 64-bit floats or as linear memory addresses (pointers). This comes with an overhead of manually interpreting these values and performing runtime type checks.

Linear Memory Management: Use linear memory to store the data and build a system around it to manage object shapes, types, and values. This essentially means creating your own dynamic type system on top of WebAssembly’s statically typed system.

Reference Types: WebAssembly's reference types proposal, which is in the process of being standardized, introduces new types such as externref. This type can be used to represent a reference to any value hosted by the WebAssembly runtime, potentially including dynamic types.

Opaque Handles: Use opaque handles (e.g., indexes or unique identifiers) to refer to data whose structure is known only to the higher-level language runtime. This means you'll perform all type-specific operations on the host side (for example, in JavaScript if you’re running in a web environment).

Anyref and Type Indices: Utilize the anyref type, a future aspect of WebAssembly's reference types proposal, as a catch-all type to represent any kind of data. Alongside anyref, you might use an index into a runtime-maintained table that maps each reference to its runtime type information for type checking and dispatch.
[11:44 AM]
BOT
 Snek:
Runtime Type Checks: Implement your own tagging system for dynamic types, followed by manual type checks at runtime. This approach often combines a value with a type tag expressed as an additional value or bit pattern.

Host Functions: Leverage host functions (functions that are implemented outside of WebAssembly and passed into it) for operations that would benefit from the host environment's dynamic typing.

Implementing dynamic types adds a runtime overhead because of the additional layer of abstraction and the need for manual type inspection and dispatch. When designing a system that interfaces WebAssembly with dynamic types, it’s important to balance this overhead with the benefits of WebAssembly’s performance and security model. Keep in mind that these techniques are often more complex than working with WebAssembly's built-in primitive types, and each comes with its trade-offs in terms of performance, maintainability, and complexity.
