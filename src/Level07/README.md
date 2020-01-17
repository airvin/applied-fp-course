# Level 07

This exercise involves extending our custom monad with some new functionality.

Since nearly any given request handler will likely require access to either the general
application configuration or the database, it is tedious to have to pass the information
in manually in every instance (e.g. in `DB.hs` firstAppDB is passed into closeDB,
getComments, addCommentToTopic, etc.). As with the abstracting away the tedium of error handling,
so we will abstract away the tedium of passing around a common structure (firstAppDB).

The steps for this level:

1. `src/Level07/AppM.hs`
2. `src/Level07/DB.hs`
3. `src/Level07/Core.hs`
