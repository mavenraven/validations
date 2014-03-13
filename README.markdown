digestive-functors-validations
==================

What
----

digestive-functors-validations is a library that adds common validations
to form inputs such as phone numbers, currency, date and times, social
security numbers, and more. It also provides localized error messages for 
invalid inputs, which can be used in conjuction with the accept headers in
Snap to provide out of the box internationalization.  It's based on the idea of
[Active Record validations](http://edgeguides.rubyonrails.org/active_record_validations.html)
, but without all the callbacks. Though this library is aimed at working
digestive-functors, it should be reusable in other contexts as well.

Help
----

To build:

git clone https://github.com/mavenraven/digestive-functors-validations

cd digestive-functors-validations

cabal sandbox init

cabal install --enable-test

Only phone numbers are working so far, so there is plenty to be worked on.
Or, if you don't want to write code but know a foreign language, it would
be great to add what you know to any needed localized strings. Thanks for
any help!


Related Projects
----------------

[Snap](https://github.com/snapframework)

[digestive-functors](https://github.com/jaspervdj/digestive-functors)

Credits
-------

Authors:

- [mavenraven.org](http://www.mavenraven.org/)
