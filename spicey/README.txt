Welcome to the Spicey web application framework!

To generate an application, follow the steps below.

1. Create a term file describing your entity-relationship model
   (see the file "Blog.erdterm" as an example).

2. Change to the directory in which you want to create the project.

3. From there execute `curry spiceup` and supply the name of the term file,
   e.g.,

       curry spiceup .../Blog.erdterm

   This generates the complete source code of the initial application
   (see the generated file README.txt for some explanations).

   You can also provide a path name, i.e., the name of a directory,
   where the database files should be stored, e.g.,

       curry spiceup --dbpath DBDIR .../Blog.erdterm

   If the parameter "--dbpath DBDIR" is not provided, then DBDIR is the
   current directory ("."). Since will be used in the _generated_ cgi
   programs, a relative directory will be relative to the place where
   the cgi programs are stored.

4. Compile the generated programs by `make compile`.

5. Configure the Makefile (variable WEBSERVERDIR) and execute
   `make deploy` to deploy the web application.

6. After the successful compilation, the application is executable
   in a web browser by loading `<URL of web dir>/spicey.cgi`.
