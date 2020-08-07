# Kelly

Kelly is a chatbot using the open source [Cerebro](https://github.com/inbrainz/cerebro) database for content. It's a very naive implementation, but it works as a simple example of how to use the Cerebro database.

It's implemented in Haskell, hence the name.

# Requirements

To use Kelly you will need [sqlite](https://sqlite.org/index.html) and [Stack](https://docs.haskellstack.org/en/stable/README/) installed on your system.

# Setup

To get this going you will have to clone this repository:

```
git clone https://github.com/guimcaballero/kelly.git
```

Then build the project with:

```
stack build
```

Finally, run with:

```
stack exec kelly-exe
```

This will start the chatbot in the command line. You can quit at any time by pressing `Ctrl+C` or by typing "quit".

# Updating the database

If you want, you can get the latest version of Cerebro in sqlite form, and add it to the project root, replacing the current one.

# Contributing

Feel free to Contribute to this project!
