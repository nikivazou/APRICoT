# SigBovik Conference Management System

## Setup

### Stack
We are using [stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/)
to build the project and manage its dependencies. You'll need stack to work on this
project.

### MySQL
We are using MySQL to handle our database. If you are using a Mac and have
`brew` installed, you can install MySQL with `brew install mysql`.

You'll need to set up a MySQL server to run locally while you develop (for now).
You can login to your MySQL server by running `mysql -u root -p`. You'll need to
make a DB for this app. We expect to find a DB named `conference_management_system`
and to be able to login as `conference_management_system@localhost` with
`conference_management_system` as the password.

### Yesod
We are using a web framework for Haskell called [Yesod](http://www.yesodweb.com/).
You can follow the instructions [here](http://www.yesodweb.com/page/quickstart) to
get started with Yesod.

## Running Locally
After cloning the repo, you'll need to install all the dependencies. Stack will do
that for you if you run `stack build`.
Yesod makes it really easy to run the project locally. All you'll need to do is run
`stack exec yesod devel`, and then visit [http://localhost:3000/](http://localhost:3000/).
