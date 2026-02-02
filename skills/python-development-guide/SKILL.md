# Python Development Guide

Orientation for Python Developers

Whether you are a data scientist, backend developer, or integration engineer, Python is likely a key part of your toolkit. With InterSystems IRIS® data platform, you can bring the full power of Python into a high-performance, multi-model data platform without compromising on speed, scalability, or fle xibility.

This documentation is a guided journey into using Python with InterSystems IRIS. Whether you are embedding Python directly into InterSystems IRIS logic, building external applications that connect to InterSystems IRIS, or implementing advanced analytics and machine learning, this guide will help you get started and grow your skills.

### 1.1 What You Will Learn

- Why InterSystems IRIS is a powerful combination for modern data-driven applications.

- How to use Python to query InterSystems IRIS like a relational database using tools like DB-API and SQLAlchemy.

- How to build familiar Python applications like REST APIs with Flask, notebooks with Jupyter, and dashboards with
Streamlit.

- How to use the many distinct approaches to Python development with InterSystems IRIS and when to apply each one.

- How to embed Python inside InterSystems IRIS for advanced logic, automation, and hybrid ObjectScript-Python workflo ws.

- How to create integrations using Python through interoperability productions.

- How to fully utilize InterSystems IRIS’s special features like multi-model capabilities, vector search, and the unique power of globals.

### 1.2 Your Python Journey Starts Here

This guide is structured to support you, whether you are just getting started with InterSystems IRIS Python development or are looking to deepen your expertise. Dive in and unlock what is possible when Python meets InterSystems IRIS.

Introduction to Python InterSystems IRIS
Development

### 2.1 What Is InterSystems IRIS?

If you are a Python developer, you are probably used to stitching together multiple tools to build data-driven applications. Perhaps you used a database here, an API layer there, some analytics tools, and a few scripts to glue it all together. Inter- Systems IRIS® data platform, is a data platform that brings all of that into one place without forcing you into a new language or way of thinking.

InterSystems IRIS is a developer-friendly data platform that combines:

- A high-performance, multi-model database including relational, document, object, key-value, etc.

- Built-in analytics and machine learning support.

- A full interoperability engine for connecting systems and services.

- Native support for Python, providing access to the extensive collection of libraries and the option to run scripts in both embedded and external processes.

And it is all designed to scale, from small applications to mission-critical systems used in healthcare, finance, and more.

### 2.2 Why InterSystems IRIS is Great for Python Developers

Python developers benefit from using InterSystems IRIS by:

- Using the tools you already know: Connect with InterSystems IRIS using SQLAlchemy, DB-API, Flask, Jupyter, Streamlit, and more.

- Not needing to move data around: InterSystems IRIS minimizes the need for complex ETL pipelines by letting you run analytics and Python logic directly on live operational data.

- Accessing data fle xibly through multi-model: Store and access data as SQL tables, JSON documents, objects, or even multi-dimensional globals all in one place.

- Utilizing a built-in Python engine: You can embed Python directly into InterSystems IRIS logic or call InterSystems IRIS from your external Python applications.

Introduction to Python InterSystems IRIS Development

- Creating interoperability productions easily: InterSystems IRIS includes tools to connect to other systems, transform data, and orchestrate workflo ws without needing a separate integration platform.

### 2.3 What You Can Build with InterSystems IRIS

With InterSystems IRIS and Python, you can build the following:

- REST APIs and microservices.

- Real-time dashboards and data applications.

- Machine learning pipelines.

- Generative AI-powered applications.

- System integrations and automations.

- Scalable, analytics-driven applications.

You do not have to learn a whole new ecosystem to get started. If you know Python, you already have the skills to build with InterSystems IRIS. This platform just gives you more power, performance, and fle xibility to do it all in one place.

Basic SQL via Python: Using InterSystems IRIS as a Relational Database

As a Python developer, you are likely familiar with querying databases using SQL. InterSystems IRIS® data platform supports this workflo w seamlessly, allowing you to treat it like a high-performance relational database using familiar Python tools. However, if you truly want to work with relational data using Python’s object-oriented nature, you can use an Object Relational Mapping (ORM), which converts data representation between a relational database and an object-oriented programming language.

This section introduces two common data manipulation techniques in Python that can be used with InterSystems IRIS:

- DB-API: A lightweight, direct SQL interface.

- SQLAlchemy: A powerful ORM and SQL toolkit.

### 3.1 Prerequisites

Before you begin, you will need to install the official InterSystems IRIS DB-API dri ver (intersystems-irispython).
Make sure to install the following packages:

pip install intersystems-irispython pip install sqlalchemy-iris

Note:

sqlachemy-iris will automatically install sqlachemy as a dependency.

If you previously installed intersystems-irispython you made need to force an update:

pip install intersystems-irispython==<version>

If you are using a local InterSystems IRIS installation, you may also need to install the intersystems-irispython wheel (.whl) file that comes with InterSystems IRIS. This ensures that the DB-API driver is installed.

pip install intersystems-irispython-<version>.whl

This .whl file is typically located in the dev/python directory of your InterSystems IRIS installation (for example,
C:\InterSystems\training\dev\python). To install it:

1. Open a command line.

2. cd into the bin directory of your InterSystems IRIS installation (for example, C:\InterSystems\training\bin).

Basic SQL via Python: Using InterSystems IRIS as a Relational Database

3. Run pip install pointing to the .whl file.

Note:

The DB-API driver (intersystems-irispython) is required even when using SQLAlchemy, as it provides the underlying connection.

### 3.2 DB-API: Direct SQL Access

The iris module provides a PEP 249-compliant interface for executing raw SQL queries. DB-API is the standard interface to interact with any relational backend. It is ideal for lightweight scripts, data access layers, and quick prototyping.

Note:

There are multiple iris modules, each with their own APIs. This section focuses on the DB-API approach, which is used for external Python applications that connect to InterSystems IRIS.

For complete documentation on the InterSystems implementation of DB-API, including InterSystems-specific extensions, see Using the Python DB-API.

#### 3.2.1 Establishing a DB-API Connection

To establish a connection to an InterSystems IRIS instance using DB-API, use the iris.connect() method. This code creates a connection to the InterSystems IRIS instance and opens a cursor for executing SQL commands.

Python

import iris

# Replace with your connection details
connection_string = "localhost:1972/USER" username = "_system" password = "SYS" connection = iris.connect(connection_string, username, password)

cursor = connection.cursor()

Note:

In connection_string, 1972 is the port number while USER is the namespace that you connect to. Change these to match your specific needs, as well.

Remember to close the cursor and connection when you are done:

Python

cursor.close() connection.close()

#### 3.2.2 Executing a SQL Query

Once connected, you can execute SQL queries using the cursor object. You can then retrieve the results from the query using the methods fetchone(), fetchmany(), fetchall(), or scroll().

Python

cursor.execute("SELECT * FROM Sample.Person WHERE Age >= 50")

row = cursor.fetchone() while row is not
None:
print(row[:]) row = cursor.fetchone()

In the above example, fetchone() returns a pointer to the next row, or None if no more data is available.

#### 3.2.3 Parameters

Parameters help prevent SQL injections and can make your queries more fle xible. With DB-API, you can specify both positional and named parameters to extend your queries. Pass the parameters along with the SQL statement to the cursor to execute them.

##### 3.2.3.1 Positional Parameters

Positional parameters match the question marks in the SQL statement with the arguments in the parameters list by position.

Python

sql = "SELECT * FROM Sample.Person WHERE ID = ? and Name = ?" params = [1, 'Doe, John'] cursor.execute(sql, params) result = cursor.fetchone() row = result[:] print(row)

##### 3.2.3.2 Named Parameters

Named parameters match the :argument variables in the SQL statement with the arguments in the parameters dictionary by keyword.

Python

sql = "SELECT * FROM Sample.Person WHERE ID = :id and Name = :name"
params = {'id' : '1', 'name' : 'Doe, John'}
cursor.execute(sql, params) result = cursor.fetchone() row = result[:] print(row)

For more documentation on DB-API, see Using the Python DB-API.

### 3.3 SQLAlchemy: SQL Toolkit and ORM

SQLAlchemy is a powerful SQL toolkit and ORM tool built on top of DB-API. It provides a higher-level abstraction over SQL, allowing you to define tables as Python classes and interact with them using ORM. It is ideal for lar ger applications that require cleaner, more maintainable code and helps you avoid raw SQL when possible.

SQLAlchemy has two major components:

1. Core—for executing raw SQL and building SQL expressions.

Basic SQL via Python: Using InterSystems IRIS as a Relational Database

2. ORM—for mapping Python classes to database tables and managing transactions.

#### 3.3.1 Establishing an Engine Connection

To connect to InterSystems IRIS using SQLAlchemy, use the create_engine() function with the appropriate dialect and connection string. The dialect is specified before the :// in the DATABASE_URL. The engine is the central source of connection and is used for both the Core and ORM.

Python

from sqlalchemy import create_engine

# Replace with your credentials and connection information
username = "_SYSTEM" password = "SYS" namespace = "USER"
DATABASE_URL = f"iris://{username}:{password}@localhost:1972/{namespace}"

engine = create_engine(DATABASE_URL, echo=True) # Set echo=True to see the SQL queries being executed in the terminal

Note:

You can also use iris+emb:// or iris+intersystems as the dialect to use Embedded Python (to run Python in the same process as InterSystems IRIS) or the official InterSystems official dri sqlalchemy-iris for more details.

ver, respectively. See

Just like in DB-API, you can execute simple static queries via the .execute() method:

Python

# SQL statements get wrapped in text sequences
from sqlachemy import text

# Connect to the database and execute a static query
with engine.connect() as conn:
query = text("SELECT * FROM Sample.Person WHERE ID = 1") result = conn.execute(query) row = result.fetchone() print(row)

#### 3.3.2 Transactions and Sessions

InterSystems IRIS supports robust transaction management through SQLAlchemy, whether you are using the Core or ORM interface. Through this, you have control over transaction states, commits, roll backs, and much more.

##### 3.3.2.1 engine.begin()

The engine.begin() method is a context manager that starts a transaction. It starts a transaction block that automatically commits when the block exists without an error.

Python

# engine.begin starts a transaction block with an auto-commit
with engine.begin() as conn:
result = conn.execute(text("SELECT Name, Age, FROM Sample.Person"))
for row in result:
print(f"Name: {row.Name}\nAge: {row.Age}\n")

##### 3.3.2.2 engine.connect()

The engine.connect() starts a transaction block that automatically rolls back when the block exists. To save any changes made in the transaction, you must call to conn.commit() to commit your changes.

Python

# engine.connect starts a transaction block with an automatic roll back
with engine.connect() as conn:
conn.execute(demo_table.insert(), [{"id": 1, "value": "Test"}],)
conn.commit() # Need to explictly commit to save changes

##### 3.3.2.3 Session(engine)

Then Session object provides a high-level interface for managing transactions and interacting with ORM objects. It establishes a conversation between the database and your code, giving you fine-grained control o ver commits, rollbacks, and object states.

Python

# Session starts an ORM session with fine-grained commit/rollback controls
from sqlalchemy.orm import Session

print("Listing rows where Age > 50:") stmt = text("SELECT Name, Age FROM Sample.Person WHERE Age > :min_age ORDER BY Age, Name")
with Session(engine) as session:
result = session.execute(stmt, {"min_age", 50})
for row in result:
print(f"Name: {row.Name}\nAge: {row.Age}\n")

#### 3.3.3 SQLAlchemy Core: Fine-Grained SQL Control

SQLAlchemy Core provides a schema-first, lo wer-level approach for building and executing SQL statements using Python
constructs. It gives you precise control over SQL generation and execution making it ideal for:

- Complex or dynamic SQL queries.

- Performance-sensitive applications.

- Developers who prefer SQL-like control without raw strings.

##### 3.3.3.1 Defining Tables with SQL Alchemy Core

You can define tables using the Table() and Column() constructors and then execute SQL statements using the select(), insert(), update(), and delete() functions. Remember to first create the table metadata. This allows SQLAlchemy to keep track of the table structure and determine whether it needs to create the table in the database when calling create_all() on the metadata object.

Python

from sqlalchemy import Column, MetaData, Table from sqlalchemy.sql.sqltypes import Integer, VARCHAR

# Create a table metadata
metadata = MetaData()

# Define the table structure
demo_table = Table( "demo_table", metadata, Column("id", Integer, primary_key = True, autoincrement = True), Column("value", VARCHAR(50)), )

# Insert sample data
with engine.connect() as conn:
conn.execute( demo_table.insert(), [
{"id": 1, "value": "Test"},
{"id": 2, "value": "More"},
], )

Basic SQL via Python: Using InterSystems IRIS as a Relational Database

conn.commit() result = conn.execute(demo_table.select()).fetchall() print("result:", result)

Note:

Tables created using this method will be seen as SQL.User.<table_name> under the InterSystems IRIS
Management Portal.

#### 3.3.4 SQLAlchemy ORM: Object-Oriented Data Access

The ORM layer abstracts away the relational structure of your data allows you to define tables as Python classes. In this way, you work with data in a more “Pythonic” way as you and interact with data using traditional Python objects.

It is ideal for:

- Create, read, update, delete (CRUD) operations.

- Business logic encapsulation.

- Applications that benefit from abstraction.

##### 3.3.4.1 Creating ORM Models

ORM allows you to define database tables as Python classes. Each class represents a table, and each attrib ute represents a column. This abstraction simplifies database operations and inte grates well with Python applications. All tables inherit from the Base class (which itself inherits from DeclarativeBase).

Python

from typing import List, Optional from sqlalchemy import String, ForeignKey from sqlalchemy.orm import DeclarativeBase, Mapped, mapped_column, relationship

class Base(DeclarativeBase):
pass

class User(Base):
__table__name = "user_account"

# 'id' column is the primary key of the table
id: Mapped[int] = mapped_column(primary_keys=True)

# 'name' column has a maximum string length of 30 characters
name: Mapped[str] = mapped_column(String(30))

# 'fullname' column is optional, so it can be NULL in the database
fullname: Mapped[Optional[str]]

# String representation of the object; useful for debugging
def __repr__(self) ->:
return f"User(id = {self.id!r}, name = {self.name!r}, fullname = {self.fullname!r})"

The general format for defining an object is:

Python

columnName: Mapped[type] = mapped_column(db.Type, arguments)

##### 3.3.4.2 Saving Data

To save data using ORM, create an instance of the model class and add it to a session. Then commit the session to persist the changes to the database.

Python

Base.metadata.create_all(engine) # Create the table if it does not exist

with Session(engine) as session:
# Create new instances of the User class
frodo = User( name = "frodo", fullname = "Frodo Baggins" ) samwise = User( name = "samwise", fullname = "Samwise Gamgee" ) pippin = User( name = "pippin", fullname = "Pippin Took")

# Add all three new User objects to the session (prepares them to be inserted)
session.add_all([frodo, samwise, pippin])

# Commit the session; writes the changes (inserts) to the database
session.commit()

##### 3.3.4.3 Retrieving Data

To retrieve data using ORM, use the Session object to query the model class. You can use filters and other query methods to refine your search results.

Python

from sqlalchemy import select # 'select' is used to build SQL SELECT queries

with Session(engine) as session:
# Print users with specific names
stmt = select(User).where(User.name.in_(["frodo", "samwise"]))

# 'scalars()' extracts the actual User objects from the result
for user in session.scalars(stmt):
print(user)

##### 3.3.4.4 Deleting Data

To delete records using SQLAlchemy ORM, you first query the object you w ant to remove, then pass it to the session’s delete() method. Finally, commit the session to apply the change to the database.

Python

with Session(engine) as session:
# List of IDs to delete
ids_to_delete = [1, 2, 3]

for user_id in ids_to_delete:
user = session.get(User, user_id)
if user:
session.delete(user) session.commit()

Building InterSystems IRIS Applications with Familiar Python Tools

Once you are comfortable querying InterSystems IRIS® data platform using SQL, you can start building interactive applications that work with your data. This section introduces how to build simple apps using familiar Python tools such as Jupyter Notebooks, Streamlit, and Flask.

These tools are widely used in the Python ecosystem and integrate well with InterSystems IRIS, allowing you to:

- Explore and visualize data from InterSystems IRIS interactively.

- Build web applications and dashboards.

- Expose InterSystems IRIS data via RESTful APIs.

### 4.1 Prerequisites

Before following this guide, make sure that you have completed the required installations in DB-API prerequisites.

### 4.2 Interactive Exploration with Jupyter Notebooks

Jupyter Notebooks are interactive coding environments often used for data science, machine learning, and teaching. They combine code, visualization, and markdown-based documentation in a single browser-based interface. With your notebooks, you can connect to InterSystems IRIS and analyze data using standard Python libraries such as pandas, matplotlib, and more. Using Jupyter Notebooks with InterSystems IRIS is no different than using any other database. Watch Using Embedded Python as a Jupyter Notebook Server to see an example of this integration.

### 4.3 Building Interactive Apps with Streamlit and InterSystems IRIS

Streamlit is a Python framework that allows developers to build interactive web applications with minimal code. You do not need to know HTML, CSS, or JavaScript. When paired with InterSystems IRIS and SQLAlchemy, it becomes a powerful tool for creating data-driven dashboards, query interfaces, and lightweight front ends.

This section walks through a complete Streamlit application that connects to InterSystems IRIS, runs SQL queries, visualizes data, and uploads CSV files into the database.

#### 4.3.1 Prerequisites

Before you begin, install streamlit in addition to the setup from before:

pip install streamlit

This particular application uses Pandas to help parse data more easily and Plotly Express to create interactive visualizations.
Make sure to install them as well for this application:

pip install pandas plotly

You should also have access to a running InterSystems IRIS instance and valid connection credentials (host, port, username, password, namespace).

#### 4.3.2 Building Your First Streamlit InterSystems IRIS App

In this section, you will build a fully functional Streamlit application that connects to InterSystems IRIS to query data, create visualizations, and even allow to file uploads to update your InterSystems IRIS database—all using Python.

You will create a lightweight, interactive web interface where users can:

- Connect to an InterSystems IRIS database and run custom SQL queries.

- View query results in an interactive table.

- Download results as CSV files.

- Visualize numeric data with Plotly charts.

- Upload CSV files and insert their contents into e xisting InterSystems IRIS tables.

This app is great for data analysts, data engineers, or developers who want a quick and intuitive interface for working with their InterSystems IRIS data, without needing to build a full web front end from scratch.

#### 4.3.3 What You Will Build (at a Glance)

Here is what the final Streamlit app look lik e:

#### 4.3.4 Connecting to InterSystems IRIS

To connect to InterSystems IRIS using SQLAlchemy, define a connection string and create an engine. This engine object will be reused throughout the app to run queries and insert data and acts like a persistent, reusable pipeline to your database.

Python

from sqlalchemy import create_engine

# Replace with your credentials and connection information
username = "_SYSTEM" password = "SYS" namespace = "USER"
DATABASE_URL = f"iris//{username}:{password}@localhost:1972/{namespace}"

engine = create_engine(DATABASE_URL, echo=True) # Set echo=True to see the SQL queries being excuted in the command line

For more documentation on using SQLAlchemy with InterSystems IRIS, see InterSystems IRIS and SQLAlchemy.

#### 4.3.5 Configuring the Streamlit App Interface

Initialize the Streamlit app by setting the page configuration and title. This helps customize the app layout and metadata.

Python

import streamlit as st

st.set_page_config(page_title="IRIS Data Explorer") st.title("InterSystems IRIS Data Explorer")

set_page_config() allows you to customize the app’s layout and metadata.

#### 4.3.6 Inputting Queries

Give users a text box to input SQL queries. You can provide an example query to help them get started.

Python

st.header("Run SQL Query")

# Create a text area where users can input to input their SQL query
# The second argument is the default query shown in the box
query = st.text_area("Enter SQL query:", "SELECT TOP 10 * FROM Sample.Person")

This lets users interactively explore any part of the database they have access to.

#### 4.3.7 Executing SQL Queries

When users click the “Execute Query” button, they run the query against your InterSystems IRIS database using engine.connect() and store the result in memory using st.session_state. The engine.connect()creates a connection between Streamlit and your data using SQLAlchemy (see InterSystems IRIS and SQLAlchemy for more details on this connection). Understanding the underlying mechanism in engine.connect()is not necessary to complete this application. Just know that you need a way to communicate between Streamlit and InterSystems IRIS. Using engine.connect() accomplishes that.

Python

import pandas as pd

# Create a button labeled "Execute Query"
# When clicked, the code inside the if-block will run
if st.button("Execute Query"):
try:
# Open a connection to the InterSystems IRIS database using SQLAlchemy
with engine.connect() as conn:
# Use pandas to execute the SQL query and load the result into a pandas DataFrame
df = pd.read_sql(query, conn)

st.success("Query executed successfully")

# Save df in session_state
st.session_state['df'] = df

except Exception as e:
st.error(f"Error: {e}")

Streamlit reruns your script top-to-bottom on every user interaction. Using st.session_state helps you retain data like query results between runs. Without saving df into your session state, you will run into errors saying that df is undefined.

#### 4.3.8 Displaying Query Results in a Table

Show the query results in an interactive table.

Python

if 'df' in st.session_state:
df = st.session_state['df']

# Display the table in your Streamlit app
st.dataframe(df)

The dataframe command displays an interactive table of your data. It allows you to sort, filter , and scroll through the table, all within the browser.

#### 4.3.9 Downloading Query Results as CSV Files

Let users export their results as a CSV file through a do wnload button widget.

Python

if 'df' in st.session_state:
csv = st.session_state['df'].to_csv(index=False).encode("utf-8")

st.download_button(
label="Download CSV",
data=csv, file_name="results.csv" mime="text/csv" )

In the example above:

- index=False removes the DataFrame’s index column from the CSV file.

- UTF-8 encoding ensures broad compatibility.

- The MIME type tells the browser that this is a CSV file.

This is useful for offline analysis or sharing results with others.

#### 4.3.10 Visualizing Data

Plotly Express is a powerful graphing library that integrates well with Streamlit for generating all sorts of charts. By feeding the data you have been extracting into Plotly, you can produce nice visualizations in web browser, all within Streamlit. Regardless of what library you use (matplotlib, ggplot2, seaborn, etc.), you can use InterSystems IRIS and Streamlit to add visualizations to your applications.

##### 4.3.10.1 Retrieving and Validating Data

Before rendering any chart, we need to:

- Check if query results (df) are available.

- Extract only numeric columns (since visualizations depend on numerical data).

Python

# Check if a DataFrame from a previous query exists
if 'df' in st.session_state:
df = st.session_state['df'] # Get the stored DataFrame

# Identify numeric columns for plotting
numeric_cols = df.select_dtype(include='number').columns.tolist()

# Warn the user if there is nothing numeric to chart
if df.empty or not numeric_cols:
st.info("No numeric data available for visualization.")

Use select_dtypes(include='number') to filter numeric columns, which are required for charts lik e line, bar, and scatter plots.

##### 4.3.10.2 User Input—Column and Chart Type Selection

Once numeric data is available, the user can:

- Choose which numeric columns to plot.

- Choose the type of chart they want to render.

Python

else:
st.subheader("Interactive Chart")

# Multiselect input to choose numeric columns to plot
cols = st.multiselect( "Select numeric columns to plot", options=numeric_cols, default=[numeric_cols[0]] # Preselect the first numeric column )

# Dropdown menu to select the chart type
chart_type = st.selectbox( "Select chart type", ["Line", "Bar", "Area", "Scatter"] )

Use st.multiselect() to let users plot multiple columns at once. For scatter plots, ensure they choose exactly 2 columns.

Note:

The else statement comes from the fact that we first check ed that there is some numeric data to chart from above.

##### 4.3.10.3 Generating Chart

Based on the user’s inputs, we create the appropriate chart using Plotly Express.

Python

import plotly.express as px

# Only proceed if columns are selected
if cols:
fig = None

if chart_type == "Line":
fig = px.line(df, y=cols, title="Line Chart of Selected Columns")

elif chart_type = "Bar":
fig = px.bar(df, y=cols, title="Bar Chart of Selected Columns")

elif chart_type == "Area":
fig = px.area(df, y=cols, title="Area Chart of Selected Columns")

elif chart_type == "Scatter":
if len(cols) >= 2:
fig = px.scatter( df, x=cols[0], # First selected column as x-axis y=cols[1], # Second selected column as y-axis
title=f"Scatter Plot: {cols[0]} vs {cols[1]}"
)
else:
st.warning("Select at least 2 columns for Scatter plot")

##### 4.3.10.4 Rendering Chart

Finally, if a figure w as successfully created, we render it in the Streamlit app.

Python

# Render the Plotly figure inside the streamlit app
if fig:
st.plotly_char(fig, use_containter_width=True)

#### 4.3.11 Uploading CSV Data into InterSystems IRIS

Users can upload a CSV file through a Streamlit app and insert its contents into a pre-e xisting InterSystems IRIS table using SQLAlchemy.

##### 4.3.11.1 Uploading a CSV

Give users the option to upload a CSV in the Streamlit app.

Python

st.subheader("Uplaod CSV to IRIS") upload_file = st.file_loader("Choose a CSV file", type="csv")

- st.file_uploader allows users to upload files through the user interf ace.

- The file type is restricted to .csv to ensure format consistency.

##### 4.3.11.2 Previewing the Data

Once the file is uploaded, use pandas to read it and sho w a preview of it.

Python

if uploaded_file:
csv_df = pd.read_csv(uploaded_file) st.dataframe(csv_df.head())

- pandas.read_csv() parses the file.

- The first fe w rows of the uploaded file are displayed with .dataframe() for review before insertion.

##### 4.3.11.3 Inputting Target Table Name

Ask the user to enter the InterSystems IRIS table name (for example, Sample.Person).

Python

# Input: full table name in format "Namespace.Table"
full_table_name = st.text.input("Enter target IRIS table (e.g., Bank.Account)")

- Users enter the full InterSystems table name, optionally including the namespace (for example, Bank.Account).

- If a dot (.) is present, it is interpreted as schema.table.

##### 4.3.11.4 Inserting Data into InterSystems IRIS

Now insert the DataFrame into the specified table.

Python

if st.button("Insert Data"):
try:
# Split schema (namespace) and table name if dot notation is used
if '.' in full_table_name:
schema, table_name = full_table_name.split('.', 1)
else:
schema = None table_name = full_table_name

# Attempt to insert the data into an existing table
with engine.begin() as conn:
csv_df.to_sql( name=table_name, con=conn, if_exists='append', index=False, schema=schema, method='multi' # Batch insert to InterSystems IRIS )

st.success(f"Successfully inserted data into {full_table_name}")

except Exception as e:
st.error(f"Insertion failed: {e}")

- The input is parsed to separate the schema (namespace) and table name.

- If no namespace is provided, schema=None is passed, and InterSystems IRIS uses the default namespace.

- engine.begin() ensures that the operation runs within a transaction context.

- In to_sql():

–

–

–

–

name is the table name.

schema is the InterSystems IRIS namespace (optional).

if_exists='append' prevents a table override.

method='multi' improves the performance by batching inserts.

Note:

The target table must already exist in InterSystems IRIS. This method does not create new tables.

If your table includes an auto-generated ID column, you must not try to insert into it directly unless explicitly allowed. To avoid RowID conflicts (for e xample, auto-generated ID columns), drop before ID inserting.

Python

if "ID" in csv_df.column:
st.warning("Dropping 'ID' column to let IRIS auto-generate it.") csv_df = csv_df.drop(columns=["ID"])

#### 4.3.12 Running Your Application

Once you have finished b uilding your Streamlit app, running it locally is quick and easy.

Save your complete code in a file (for e xample, app.py), and then your Streamlit app from the command line:

streamlit run app.py

If the streamlit is not recognized, you can also run it as a Python module:

python -m streamlit app.py

Your browser should automatically open the app. The command line will display logs, status updates, and the URL to access your app manually. You can now interact with your app live, whether you are running queries, uploading files, or visualizing data.

When you make changes to your app.py file:

- Simply save the file, and Streamlit will detect changes.

- By default, the Streamlit app prompts you to rerun it. Click “Always rerun” (in the top-right corner of the browser) for a smoother workflo w.

Visit the official Streamlit, pandas, and Plotly documentation to explore more available capabilities.

#### 4.3.13 Complete Streamlit Code

The following is the entire code for the built Streamlit application.

Python

# Initializing Streamlit app
import streamlit as st

import pandas as pd

# Import Plotly Express for interactive charting
import plotly.express as px

# SQLAlchemy Connection to InterSystems IRIS
from sqlalchemy import create_engine

# Connect to IRIS
username = "superuser" password = "SYS" namespace = "USER"

DATABASE_URL = f"iris://{username}:{password}@localhost:1972/{namespace}"

engine = create_engine(DATABASE_URL, echo=True)

st.set_page_config(page_title="IRIS Data Explorer") st.title("InterSystems IRIS Data Explorer")

st.header("Run SQL Query")

# Create a text area where users can input their SQL query
# The second argument is the default query shown in the box
query = st.text_area("Enter SQL query:", "SELECT TOP 10 * FROM Sample.Person")

# Create a button labeled "Execute Query"
# When clicked, the code inside the if-block will run
if st.button("Execute Query"):
try:
# Open a connection to the IRIS database using SQLAlchemy
with engine.connect() as conn:
# Use pandas to execute the SQL query and load the result into a DataFrame
df = pd.read_sql(query, conn)

st.success("Query executed successfully!")

# Display the resulting DataFrame as an interactive table in the app
st.dataframe(df)

# Save df in session_state
st.session_state['df'] = df

except Exception as e:
st.error(f"Error: {e}")

if 'df' in st.session_state:
csv = st.session_state['df'].to_csv(index=False).encode("utf-8")

st.download_button(
label="Download CSV",
data=csv, file_name="results.csv", mime="text/csv" )

# Check if a DataFrame from previous query exists
if 'df' in st.session_state:
df = st.session_state['df'] # Get the stored DataFrame

# Identify numeric columns for plotting
numeric_cols = df.select_dtypes(include='number').columns.tolist()

# Warn the user if there's nothing numeric to chart
if df.empty or not numeric_cols:
st.info("No numeric data available for visualization.")

# Check if df is stored in session_state and visualize
if 'df' in st.session_state:
df = st.session_state['df']

# Check if the query results DataFrame ('df') exists in Streamlit's session_state
if 'df' in st.session_state:
df = st.session_state['df'] # Retrieve the DataFrame from session state

# Extract only numeric columns from the DataFrame for plotting
numeric_cols = df.select_dtypes(include='number').columns.tolist()

# If no data is available or there are no numeric columns, notify the user
if df.empty or not numeric_cols:
st.info("No numeric data available for visualization.")
else:
# Display a subheader for the chart section
st.subheader("Interactive Chart")

# Allow users to select one or more numeric columns to plot
# The default is the first numeric column
cols = st.multiselect( "Select numeric columns to plot", options=numeric_cols, default=[numeric_cols[0]] )

# Let users choose the type of chart to generate
chart_type = st.selectbox( "Select chart type", ["Line", "Bar", "Area", "Scatter"] )

# Only proceed if at least one column is selected
if cols:
fig = None # Initialize the figure object

# Generate the appropriate chart based on the selected type
if chart_type == "Line":
# Plot a line chart with the selected columns on the y-axis
fig = px.line(df, y=cols, title="Line Chart of Selected Columns")

elif chart_type == "Bar":
# Plot a bar chart with the selected columns on the y-axis
fig = px.bar(df, y=cols, title="Bar Chart of Selected Columns")

elif chart_type == "Area":
# Plot an area chart with the selected columns on the y-axis
fig = px.area(df, y=cols, title="Area Chart of Selected Columns")

elif chart_type == "Scatter":
# Scatter plot requires at least 2 numeric columns
if len(cols) >= 2:
# Use first column as x-axis and second as y-axis
fig = px.scatter( df, x=cols[0], y=cols[1],
title=f"Scatter Plot: {cols[0]} vs {cols[1]}"
)
else:
st.warning("Select at least 2 columns for Scatter plot")

# If a valid figure was created, display it using Streamlit
if fig:
st.plotly_chart(fig, use_container_width=True)
# use_container_width=True makes the chart responsive to app layout

st.subheader("Upload CSV to IRIS")

# File uploader for CSV files
uploaded_file = st.file_uploader("Choose a CSV file", type="csv")

if uploaded_file:
# Read and preview the uploaded CSV
csv_df = pd.read_csv(uploaded_file) st.dataframe(csv_df.head())

# Input: full table name in format "Namespace.Table"
full_table_name = st.text_input("Enter target IRIS table (e.g. Bank.Account)")

if st.button("Insert Data"):
try:
# Split schema (namespace) and table name if dot notation is used
if '.' in full_table_name:
schema, table_name = full_table_name.split('.', 1)
else:
schema = None table_name = full_table_name

# Attempt to insert data into existing table
with engine.begin() as conn:
csv_df.to_sql( name=table_name, con=conn, if_exists='append', index=False, schema=schema, method='multi' # batch insert for IRIS )

st.success(f"Successfully inserted data into '{full_table_name}'")

except Exception as e:
st.error(f"Insertion failed: {e}")

### 4.4 Building RESTful Flask Applications with InterSystems IRIS

Flask is a lightweight and fle xible Python web framework, ideal for developing REST APIs and back-end services. When paired with SQLAlchemy and InterSystmes IRIS, Flask makes it easy to expose InterSystems IRIS data to front-end applications, services, and other consumers. This guide focuses on the InterSystems IRIS-specific setup steps so that you can get started quickly, and then proceed as with any other Flask app.

#### 4.4.1 What You Will Learn

- How to set up a basic Flask application.

- How to connect Flask to InterSystems IRIS using SQLAlchemy.

- How to use HTML templates and render data from InterSystems IRIS.

#### 4.4.2 What You Will Build

By the end of this documentation, you will have a Flask web application that allows you to view your data in InterSystems IRIS through a clean user interface in the web browser.

#### 4.4.3 Prerequisites

Before building the Flask app, make sure you have the following installed in addition to the setup from above

pip install flask

You should also have access to a running InterSystems IRIS instance and valid connection credentials (host, port, username, password, namespace).

#### 4.4.4 Creating Your First Flask App

To get started with Flask, you will create a minimal web application that returns a simple message in the browser.

First, create a new folder for your project (my_flask). Inside it, create a file named app.py with the following content:

Python

from flask import Flask, render_template

app = Flask(__name__)

@app.route("/")
def hello_world():
return "<p>Hello World!</p>"

if __name__ == '__main__':
app.run(debug=True)

In the code above:

- Flask(__name__) creates the Flask application instance.

- @app.route("/") decorator defines the route for the rool URL ( /).

- hello_world() is the view function returning a simple HTML message.

- app.run(debug=True) starts the local development server with debugging enabled.

In Flask, routings map URLs to their respective functions (as defined by @app.route("<URL>")). Accessing these URLs triggers their associated functions. The / URL calls the home function (typically at http://127:0.0.1:5000/). To call other functions via other URLs (defined by @app.route("<URL>")), open http://127:0.0.1:5000/<URL> on your browser.

Important:

Do not name your application as flask.py for doing so creates a conflict with Flask itself.

#### 4.4.5 Running the Flask Application

From the folder containing app.py, run the following in the command line:

python app.py

This starts the server, and you can now access your app at http://127:0.0.1:5000/.

#### 4.4.6 Using HTML Templates

Web applications typically use HTML for rendering pages. Flask integrates the Jinja2 templating engine to separate your Python logic from HTML code. This separation improves code readability, maintainability, and reusability.

Understanding the detailed syntax and structure of HTML, CSS, and JavaScript is out of scope for this guide. Just know that you can use them within your Flask app to build dynamic, styled, and interactive webpages.

To implement an HTML template, follow these steps:

1. Create a folder named templates the same directory as app.py.

2.

Inside templates, create a filed named index.html with this content:

<!DOCTYPE html> <html>

<body> <h1>My First Heading</h1> <p>My First Paragraph</p> </body>

<html>

3. Modify app.py to render the template.

Python

from flask import Flask

app = Flask(__name__)

@app.route("/")
def hello_world():
return render_template('index.html')

if __name__ == '__main__':
app.run(debug=True)

The following happens in the code above:

- When a user accesses the root URL (/), Flask triggers the hello_world() function.

- This function calls render_template('index.html'), which tells Flask to load and return the HTML from the templates/directory.

The templates/ folder is Flask’s default location for all HTML templates. Flask separates the front end (HTML/CSS/JS) from the back end (Python), making your codebase cleaner and easier to manage. This modular approach also makes it easier to scale your application as you add more pages and templates.

#### 4.4.7 Folder Structure

This is a typical minimal Flask application layout. Each file and folder serv e a specific role to separate the dif ferent parts of the application code and keep the project organized and maintainable.

##### 4.4.7.1 What Each Folder Component Is

- app.py

- This is the entry point of the Flask application. It contains the route definitions, application configuration, and logic to start the server. It typically handles request routing and renders templates.

models.py

Contains the SQLAlchemy model definitions, which map Python classes to database tables. This helps abstract and manage database interactions cleanly.

Note: While models.py is not strictly required, it helps organize your Object Relational Mapping logic in a clean

and modular way.

templates/

Flask uses Jinja2 templating and all HTML files go in this folder . The framework automatically looks for templates here when rendering views using render_template().

index.html

A specific HTML file inside the templates folder where your front end DataTable integration (like in render_template()) would live.

, typically used as the home page or main data table view. This is

static/

- Optional but useful for storing static files lik e custom CSS, JavaScript, images, or fonts. Flask will serve these files from the static/URL path automatically.

- Flask intentionally does not enforce strict structures, so while this layout is clean and scalable, you are free to adapt it based on your application’s needs.

- 4.4.8 Connecting to InterSystems IRIS To connect to InterSystems IRIS using SQLAlchemy, define a connection string and create an engine. This engine object will be reused throughout the app to run queries and insert data and acts like a persistent, reusable pipeline to your database.

Python

from sqlalchemy import create_engine, text

# Replace with your credentials and connection information
username = "_SYSTEM" password = "SYS" namespace = "USER"
DATABASE_URL = f"iris://{username}:{password}@localhost:1972/{namespace}"

engine = create_engine(DATABASE_URL, echo=True) # Set echo=True to see the SQL queries being excuted in the terminal

For more documentation on using SQLAlchemy with InterSystems IRIS, see InterSystems IRIS and SQLAlchemy.

#### 4.4.9 Transferring Data from InterSystems IRIS to Flask and Displaying it

##### 4.4.9.1 tables.html

The following provides the web page structure for viewing the InterSystems IRIS data. It utilizes jQuery DataTables, a popular JavaScript library used to create dynamic, interactive tables with features like pagination, sorting, searching, and responsive layouts. It also incorporates Bootstrap, a modern CSS framework that provides a responsive grid system, prestyled UI components, and utility classes. Bootstrap is used here to style the table and layout elements (such as spacing and table orders), ensuring the table looks clean and is mobile-responsive.

<!DOCTYPE html> <html>

<head> <title>Display InterSystems IRIS Data with DataTables</title>

<!-- Bootstrap 5 for styling --> <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css" rel="stylesheet">

<!-- jQuery (required by DataTables) --> <script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>

<!-- DataTables + Bootstrap 5 integration --> <link rel="stylesheet" href="https://cdn.datatables.net/1.13.5/css/dataTables.bootstrap5.min.css">

<script src="https://cdn.datatables.net/1.13.5/js/jquery.dataTables.min.js"></script> <script src="https://cdn.datatables.net/1.13.5/js/dataTables.bootstrap5.min.js"></script> </head>

<body class="container py-4">

<h2 class="mb-4">InterSystems IRIS Data Table</h2>

<!-- HTML table to be populated dynamically --> <table id="myTable" class="table table-bordered table-striped table-hover"></table>

<script>
$(document).ready(function () {
// Convert server-passed JSON strings into JavaScript objects
let my_data = JSON.parse('{{ my_data | tojson | safe }}');
let my_cols = JSON.parse('{{ my_cols | tojson | safe }}');

// Initialize DataTable with server data and column definitions
$('#myTable').DataTable({
data: my_data, columns: my_cols, responsive: true, lengthMenu: [5, 10, 25, 50], pageLength: 10,
language: {
search: "_INPUT_", searchPlaceholder: "Search records"
}
});
});
</script>

</body>

</html>

Just like with index.html, understanding the code above is not within the scope of this documentation. The takeaway here is that you can integrate these tools into your Flask application. Refer to the official documentation of these libraries to learn more about them.

##### 4.4.9.2 app.py

Create a dynamic view into your InterSystems IRIS data through the /table/ URL route. Using a connection via the SQLAlchemy engine to your InterSystems IRIS database, query data from the specified table and populate it into your Flask app. Pass the resulting data to the HTML template from above to create a clean and structured display of it in the web page.

You can specify which table to query directly through the URL. For example, to view data from a table called Bank.Account, open http://127:0.0.1:5000/table/Bank.Account on your browser.

The app will:

1. Parse the table name (optionally including the schema).

2. Execute a SELECT * FROM table ORDER BY 1 query.

3. Convert the results into a list of dictionaries.

4. Dynamically extract column names.

5. Render the tables.html template with your data and columns.

Python

@app.route("/table/<path:table_name>")
def show_table(table_name):
# table_name could be "Schema.Table" or just "Table"
try:
# Validate and split schema/table if schema is provided
if '.' in table_name:
schema, table = table_name.split('.')

full_table = f"{schema}.{table}"
else:
schema = None table = table_name full_table = table

# Query to get data from the specified table
query = f"SELECT * FROM {full_table} ORDER BY 1"

with engine.connect() as conn:
result = conn.execute(text(query)) keys = result.keys() # get column names rows = [dict(zip(keys, row)) for row in result]

if not rows:
return f"No data found in table {full_table}", 404

# Extract columns dynamically from first row keys
columns = [{"data": col, "title": col.capitalize()} for col in rows[0].keys()]

return render_template("tables.html", my_data=rows, my_cols=columns)

except Exception as e:
return f"Error: {str(e)}", 500

In the code above:

- <path:table_name> defines a Flask route where table_name can include dots (for example, Bank.Account), The :path converter allows such values to be passed as arguments into the view function.

- Flask uses the Jinja2 templating engine, which allows you to pass arguments (like rows and columns) from your
view function into your HTML templates. These variables can be inserted dynamically using the syntax {{variable}}
(see tables.html as an example).

- if '.' in table_name splits the table into schema and table components when the format includes a dot.

- rows = [dict(zip(keys,row) for row in result] passes both the row data and column definitions into the HTML template, enabling a dynamic and responsive table view.

Note:

The result returned by conn.execute() is not a dictionary. Instead, it is an iterable of row tuples or RowProxy objects. To make data easier to work with in Jinja2 templates, the code converts each row to a dictionary by pairing column names with values using zip().

This application enables you to quickly browse and visualize any table in your InterSystems IRIS database by simply modifying the URL. It dynamically pulls and formats tables using SQLAlchemy, then renders it in a styled HTML table using Jinja2. The use of dynamic route arguments and template variables makes it fle xible for inspecting a wide range of tables without modifying the back-end logic.

This documentation only touches the surface of Flask. Now that you know how to get started creating a Flask application with InterSystems IRIS, you can fully utilize all of Flask’s features just as with any other Flask application.

To dive deeper into Flask, visit the official Flask documentation.

#### 4.4.10 Complete Flask Code

The following is the code for the Flask application built. Be aware of the file structure required to ha ve the application running smoothly.

Python

#app.py

from flask import Flask, render_template from sqlalchemy import create_engine, text

# Replace with your credentials and connection information
username = "_SYSTEM" password = "SYS"

namespace = "USER"
DATABASE_URL = f"iris://{username}:{password}@localhost:1972/{namespace}"

engine = create_engine(DATABASE_URL, echo=True) # Set echo=True to see the SQL queries being excuted in the terminal

app = Flask(__name__)

# ---------- ROUTES ----------
@app.route("/")
def hello_world():
# main index page
return render_template("index.html")

@app.route("/table/<path:table_name>")
def show_table(table_name):
# table_name could be "Schema.Table" or just "Table"
try:
# Validate and split schema/table if schema is provided
if '.' in table_name:
schema, table = table_name.split('.')
full_table = f"{schema}.{table}"
else:
schema = None table = table_name full_table = table

# Query to get data from the specified table
query = f"SELECT * FROM {full_table} ORDER BY 1"

with engine.connect() as conn:
result = conn.execute(text(query)) keys = result.keys() # get column names rows = [dict(zip(keys, row)) for row in result]

if not rows:
return f"No data found in table {full_table}", 404

# Extract columns dynamically from first row keys
columns = [{"data": col, "title": col.capitalize()} for col in rows[0].keys()]

return render_template("tables.html", my_data=rows, my_cols=columns)

except Exception as e:
return f"Error: {str(e)}", 500

<!-- index.html -->

<!DOCTYPE html> <html>

<body> <h1>My First Heading</h1> <p>My First Paragraph</p> </body>

<html>

<!-- tables.html -->

<!DOCTYPE html> <html>

<head> <title>Display IRIS Data with DataTables</title>

<!-- Bootstrap 5 for styling --> <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.0/dist/css/bootstrap.min.css" rel="stylesheet">

<!-- jQuery (required by DataTables) --> <script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>

<!-- DataTables + Bootstrap 5 integration --> <link rel="stylesheet" href="https://cdn.datatables.net/1.13.5/css/dataTables.bootstrap5.min.css">

<script src="https://cdn.datatables.net/1.13.5/js/jquery.dataTables.min.js"></script> <script src="https://cdn.datatables.net/1.13.5/js/dataTables.bootstrap5.min.js"></script> </head>

<body class="container py-4">

<h2 class="mb-4">IRIS Data Table</h2>

<!-- HTML table to be populated dynamically -->

<table id="myTable" class="table table-bordered table-striped table-hover"></table>

<script>
$(document).ready(function () {
// Convert server-passed JSON strings into JavaScript objects
let my_data = JSON.parse('{{ my_data | tojson | safe }}');
let my_cols = JSON.parse('{{ my_cols | tojson | safe }}');

// Initialize DataTable with server data and column definitions
$('#myTable').DataTable({
data: my_data, columns: my_cols, responsive: true, lengthMenu: [5, 10, 25, 50], pageLength: 10,
language: {
search: "_INPUT_", searchPlaceholder: "Search records"
}
});
});
</script>

</body>

</html>

Deciding How to Use InterSystems IRIS with Python

So far, you have been only working with InterSystems IRIS® data platform through the traditional client-server architecture with the DB-API driver. While this offers familiarity and simplicity for Python developers, it limits access to many of the advanced, high-performance features that separates InterSystems IRIS from other databases.

Luckily, InterSystems IRIS offers fle xible and powerful ways to integrate Python into your data workflo ws, either via external connections (APIs, gateways, and SDKs) or within the kernel (Embedded Python). This document guides you how to go beyond DB-API and unlock the full potential of InterSystems IRIS.

### 5.1 Python Options

Not every application is the same. Each application requires features and capabilities specific to its needs. InterSystems IRIS offers many options in the ways you can implement your applications with Python depending on what you are looking for. This document summarizes the ways of using Python with InterSystems IRIS and highlights some of the use cases for each of them.

Figure 5–1: Python Support with InterSystems IRIS

#### 5.1.1 Client-Server Options

The four client-server options use language SDKs that run Python outside of the InterSystems IRIS process.

1. DB-API: Implements a PEP 249–compliant direct interface with InterSystems databases through the DB-API driver.

2.

pyODBC: Establishes an Open Database Connectivity (ODBC) connection to InterSystems IRIS through the ODBC driver.

3. Native API: Provides direct access to many InterSystems IRIS features from Python through the Native driver.

4. Python Gateway: Calls out to an external language server.

#### 5.1.2 Embedded Python

With Embedded Python, Python runs in the same process as InterSystems IRIS. Embedded Python provides the lowest latency out of all these options.

DB-API

### 5.2 DB-API

The InterSystems IRIS Python DB-API driver provides a standards-compliant interface for accessing InterSystems IRIS data from external Python applications. It is based on the widely adopted Python DB-API 2.0 (PEP 249), making it easy for developers to work with InterSystems IRIS using familiar Python database interaction patterns.

This driver comes bundled as part of the Native SDK and is distributed via a single Python package (through a wheel). It enables client-side access to InterSystems IRIS, connecting over the network to interact with data in a relational, SQL-like way.

Note:

Note:

If you are using DB-API and the client is on same machine as InterSystems IRIS, Python and InterSystems IRIS communicate using shared memory by default, for greater performance. This option can be turned off if you need Python to communicate with InterSystems IRIS using a secure connection, or to simulate connecting from another machine via TCP/IP. See Creating a Connection Object for details.

DB-API and Embedded Python both require you to import an iris module. Though the modules have some methods with similar names, they have separate APIs and are not interchangeable. DB-API uses the connect() method of the Native SDK’s iris module to create a connection object. For information, see Using the Python
DB-API.

#### 5.2.1 When to Use the DB-API Driver

- You are developing external Python applications (for example, data processing, analytics, or web services) that need to connect to InterSystems IRIS over a standard interface.

- You prefer or require working entirely with Python, outside of the InterSystems IRIS environment.

- You want to leverage the standard Python database workflo w, including connection objects, cursors, and SQL execution.

- You are integrating InterSystems IRIS with Python tools and frameworks that expect DB-API compatibility (for example, pandas, SQLAlchemy, ORMs, Flask, and Streamlit).

For more information, see Using the Python DB-API.

### 5.3 pyODBC

pyODBC is a Python module that provides a bridge between Python and databases via the ODBC standard. It implements the Python DB-API 2.0 specification and allo ws applications to connect to InterSystems IRIS using ODBC drivers.

Important: While it offers broad compatibility and has historically been used in many Python database workflo ws,

pyODBC is no longer the recommended approach for connecting to InterSystems IRIS in most cases. Instead, use the DB-API driver.

#### 5.3.1 When to Use pyODBC

- You need to connect to older versions of InterSystems IRIS (prior to version 2022.1) that do not support the Python DB-API driver.

- You are working in an environment where ODBC is already in place and integration depends on existing Data Source
Names (DSNs).

- Your workflo w depends on a low-level, ODBC-based architecture, such as certain legacy reporting or business intelligence tools that are being accessed from Python.

For more information, see ODBC Support for Python and Node.js.

### 5.4 Native API

The InterSystems IRIS Native API allows Python applications to interact directly with InterSystems IRIS through lowlevel access to its core data structures—specifically , globals, the high-performance, multi-dimensional arrays used internally by InterSystems IRIS.

This API is part of the Native SDK and enables external Python code to connect to InterSystems IRIS over the network and perform non-relational operations without using SQL. It offers fine-grained control o ver data and is well-suited for advanced or performance-critical applications.

Note:

If you are using the Native API and the client is on same machine as InterSystems IRIS, Python and InterSystems IRIS communicate using shared memory by default, for greater performance. This option can be turned off if you need Python to communicate with InterSystems IRIS using a secure connection, or to simulate connecting from another machine via TCP/IP. See Creating a Connection in Python for details.

Note:

The Native SDK and Embedded Python both require you to import an iris module. Though the modules have some methods with similar names, they have separate APIs and are not interchangeable. For information on the iris module used by the Native SDK, see Native SDK for Python Quick Reference.

#### 5.4.1 When to Use the Native API

- You need direct access to InterSystems IRIS globals, bypassing SQL and object layers.

- You are building high-performance or low-latency data processing systems that benefit from schema-less access.

- You are developing Python applications that run outside of the InterSystems IRIS environment.

- Your use case involves non-relational data models, such as hierarchical structures or key-value stores.

For more information, see Introduction to the Native SDK for Python.

### 5.5 Python Gateway

The Python Gateway, also known as the External Language Server, enables InterSystems IRIS to call out to Python code running externally, reversing the usual client-server relationship. Instead of Python initiating the connection to InterSystems IRIS, InterSystems IRIS becomes the caller, invoking Python code that resides on a separate system or process. This method establishes a communication bridge from InterSystems IRIS to Python, making it possible to integrate Python logic—such as a machine learning models, data processing routines, or specialized computations—into InterSystems IRIS workflo ws.

The gateway runs as a separate service and communicates with InterSystems IRIS over a defined protocol, allo wing Python to be part of business logic, orchestration, and process automation within the InterSystems IRIS environment.

#### 5.5.1 When to Use the Python Gateway

- You want InterSystems IRIS to initiate execution of external Python code that lives outside the database processes.

- You are integrating existing Python applications, scripts, or services into InterSystems IRIS business logic.

- Embedded Python is not available in your deployment, but Python functionality is still required.

- Your use case involves external systems or APIs that are best handled by Python but must be coordinated from within
InterSystems IRIS.

- You want to build or extend interoperability productions using Python components.

For more information, see Working with External Languages.

#### 5.5.2 PEX Framework: A Key Use Case for the Python Gateway

The Production EXtension (PEX) framework is an important example of how the Python Gateway architecture is used in practice. PEX allows you to develop interoperability productions—InterSystems IRIS workflo ws that integrate systems with different message formats and protocols—using external languages such as Python. With PEX, you can implement business services, processes, and adapters in Python that run as separate services connected via the gateway. These Python components are invoked at runtime and communicate with other production elements through the PEX messaging system, enabling seamless integration with InterSystems IRIS interoperability productions.

For more information, see Introduction to the PEX Framework.

### 5.6 Embedded Python

In the client-server setup, Python and InterSystems IRIS run in separate processes. This means that each request between them must travel across a network boundary. These requests introduce latency, require serialization, and prevent tight integration with InterSystems IRIS-specific features.

In contrast, with Embedded Python, the Python runtime and the InterSystems IRIS runtime are contained in same process. This tight integration means faster database access and streamlined communication between Python and ObjectScript, the native InterSystems procedural programming language.

Figure 5–2: Python Processes with InterSystems IRIS

The benefits of Embedded Python are:

- Performance: There is no need to serialize data between InterSystems IRIS and Python.

- Simplicity: Seamlessly integrate Python with InterSystems IRIS, and deploy your Python code together with your ObjectScript code.

- Security: There is no need to open any additional ports for communication between InterSystems IRIS and Python. You can leverage the InterSystems native security model.

- Scalability: Utilize InterSystems IRIS’s ECP and sharding features to easily scale your applications.

You can run Embedded Python in one of two basic modes: InterSystems IRIS running inside the Python process or Python running inside the InterSystems IRIS process.

If you are ready to move beyond basic Python database access and fully embrace what InterSystems IRIS has to offer, Embedded Python is the path forward.

Note:

Note:

The Native SDK and Embedded Python both require you to import an iris module. Though the modules have some methods with similar names, they have separate APIs and are not interchangeable. For information on the iris module used by Embedded Python, see InterSystems IRIS Python Module Reference.

Virtual environments are not currently supported for Embedded Python, as it uses the specific Python e xecutable specified in the Embedded Python configuration. Client-side Python code can be deplo yed in a virtual environment, and you can have multiple virtual environments on the same machine that connects to the same InterSystems IRIS process.

#### 5.6.1 InterSystems IRIS Running Inside the Python Process

In this mode, you use the command irispython to call in to InterSystems IRIS. By running irispython myscript.py from the command line, your Python script runs in the same process with InterSystems IRIS, while your Python code remains separate from any ObjectScript code. This code separation allows you to use all your customary Python development practices: such as debuggers, linters, and syntax coloring tools.

#### 5.6.2 Python Running Inside the InterSystems IRIS Process

In this mode, InterSystems IRIS calls out to Python. There are several ways to initiate Python from InterSystems IRIS,
each with specific use cases, for e xample:

- Write a method in an InterSystems IRIS class using the keyword [ Language = python ]. This is useful when you have an existing InterSystems IRIS class and you want to add a simple method written in Python.

- Use the Import() method of the %SYS.Python class in InterSystems IRIS. This is useful when you want to import a Python module from ObjectScript in order to perform a well-defined task.

- Launch the Python shell from the Terminal using the Shell() method of the %SYS.Python class. This is useful for testing a few lines of Python code interactively from within the InterSystems IRIS environment.

For larger-scale Python development, InterSystems recommends calling in to InterSystems IRIS from a .py file using irispython.

Note: When Python is running within InterSystems IRIS, it is operating in a environment with multiple processes, users,

home directories, and permissions. This results in added complexity that can sometimes make it more difficult to diagnose an issue when using Embedded Python.

### 5.7 Choosing the Right Path for You

It is important to choose the right Python option when working with InterSystems IRIS to fully take advantage of each approach. Understanding the nuances of each option can help determine which path to take. Use the following as resources to help navigate the broad Python support system that InterSystems IRIS provides.

#### 5.7.1 Python Support Capabilities

Use Case

Client Applications

SQL Stored Procedures, Functions,
Triggers

Augmenting existing InterSystems IRIS
Classes

Interoperability

Manipulating Globals

DB-API or

Native
API

Python
Gateway

Embedded
Python

Yes

No

No

No

No

Yes

No

No

No

Yes

No

Possible

No

No

Yes

Yes

Yes (PEX)

Possible

Yes

Yes

#### 5.7.2 Python Uses Cheat Sheet

What You Are Building

Recommended
Python Method

Why This Works Well

REST APIs, dashboards, client applications, or relational database access requiring SQL calls (for small amounts of data)

Applications needing direct access to InterSystems IRIS globals or non-relational data

DB-API or

Familiar SQL-based access; great for
lightweight, structured data interactions

Native API

Offers low-level access to hierarchical and multi-dimensional data structures

InterSystems IRIS logic that needs to call external Python code

Python
Gateway (PEX)

High-performance, data-intensive logic close
to the database; importing a third-party
Python module for use in an existing ObjectScript application

Embedded
Python

Allows InterSystems IRIS to trigger Python
scripts or models externally; great for
interoperability productions

Runs inside the InterSystems IRIS kernel;
lowest latency and tightest integration with
the data; makes it possible to use popular
Python packages from ObjectScript

Becoming an InterSystems IRIS Power
User

Once you are comfortable working with Python and InterSystems IRIS® data platform, you are ready to take the next step and unlock the full potential of the platform. InterSystems IRIS is far more than a database. It is a unified data platform that offers powerful, integrated capabilities across interoperability, analytics, multi-model storage, and AI-enablement. InterSystems IRIS delivers the scale and speed of modern cloud-native systems, the interoperability of enterprise middleware, and the performance of an in-process analytics platform, all in one cohesive environment.

This section introduces you to these advanced features which allow you to build smarter, faster, and more connected applications. Becoming an InterSystems IRIS power user means expanding beyond traditional development patterns and leveraging the architectural strengths of this platform.

### 6.1 Building Interoperability Productions with Python

InterSystems IRIS is a platform designed for integration and orchestration. Its interoperability features—such as message routing, persistent queues, and businesses processes—can all be extended using Python.

Using the Python Gateway and PEX framework, you can embed Python code directly into your interoperability productions.
This enables real-time interaction with Python-based services. For example:

- You can create a business service in Python that ingests data from an external API.

- You can define a b usiness process that calls a Python machine learning model to make a decision.

- You can develop a business operation in Python that pushes transformed data to an external system.

If you are building integrations between systems, Python can now be a full participant in your InterSystems IRIS production pipelines, meaning that Python code can participate in messaging, tracing, error handling, and orchestration like any native component. The gateway-based approach allows InterSystems IRIS to invoke external Python modules as part of real-time workflo ws, making it ideal for hybrid architecture or multi-system data flo ws.

Becoming an InterSystems IRIS Power User

### 6.2 Taking Advantage of InterSystems IRIS’s Multi-Model and Analytics Features

InterSystems IRIS supports multi-model data access, allowing you to store and query data using relational, object, document, and key-value paradigms all in one system. Its architecture supports real-time analytics, embedded business intelligence, vector search, and even natural language processing—all from a single platform. You combine these models to suit your application’s needs and access them from Python.

#### 6.2.1 Some Key Capabilities to Explore as a Power User

- Integrated Business Intelligence: InterSystems IRIS includes dashboards and pivot tables that you can integrate into your applications.

- Vector Search: InterSystems IRIS supports native vector storage and indexing for AI workloads, such as retrievalaugmented generation (RAG). You can build hybrid applications combining Python LLM orchestration with in-IRIS vector search for high performance using libraries such as LangChain and Hugging Face.

- Adaptive Analytics: InterSystems IRIS provides a virtual data model layer between InterSystems IRIS, Business Intelligence, and Artificial Intelligence client tools. This common data model abstracts away differing definitions and calculations, providing a unified approach to w orking with your business operations.

#### 6.2.2 Mastering Globals: Unlocking High-Performance Data Modeling

To truly harness InterSystems IRIS’s performance capabilities, it is worth understanding globals, the native multi-dimensional, hierarchical data structures at the core of InterSystems IRIS.

Globals offer:

- Schema-less, key-value styled storage with deep nesting.

- Extremely fast read-write performance even at large scales.

- Support for real-time telemetry, hierarchical document modeling, and non-relational use cases.

While not required for typical SQL-based applications, globals are key reason why many high-throughput systems run on InterSystems IRIS. Python can interact with globals via the Native API (out-of-process) or Embedded Python (in-process), making it easier to explore and prototype new storage models.

If you are building performance-critical applications, globals are an indispensable part of the InterSystems IRIS toolkit. Understanding how globals work will elevate your ability to optimize performance, customize data structures, and take full advantage of the InterSystems IRIS engine.

#### 6.2.3 ObjectScript:The Native Language of InterSystems IRIS

ObjectScript is the native language of InterSystems IRIS. Just like Python, ObjectScript is an object oriented dynamic, interpreted language with full polymorphic dispatch. Written in C, both languages use reference counting for object lifetimes. These similarities make it easy to work with the two languages together. However, once you begin exploring ObjectScript, you will find a language that is e xpressive, compact, and designed specifically for data-centric programming. Though you can utilize 90% of the distinguishing features of InterSystems IRIS by using Embedded Python, consider using ObjectScript to completely make use of InterSystems IRIS.

Moving forward

##### 6.2.3.1 What Makes ObjectScript Special

- It is optimized for data operations: Globals, objects, indexes, and storage strategies are all native to the language.

- You can write compact, high-performance logic with less code than with other languages.

- It allows seamless translation between different data storage paradigms.

##### 6.2.3.2 Embracing the Best of All Worlds: Python, ObjectScript, and Globals

Being a power user in InterSystems IRIS does not mean choosing one language or access method over another. It means knowing when and how to use each. With Embedded Python, you can combine the best of modern scripting with the unique strengths of InterSystems IRIS.

Here are some practices to consider:

- Use SQL for declarative access and compatibility.

- Use Globals for unmatched performance and fle xibility.

- Use Python for AI, modeling, data science, or custom algorithms.

- Use ObjectScript to bridge it all, especially when building classes, services, and complex business logic inside the InterSystems IRIS process.

InterSystems IRIS is unique because it does not force you to choose between modernity and performance, or openness and reliability. It invites you to build complete systems, with the freedom to code where you are most comfortable, and the power to go deeper when you need to.

### 6.3 Moving forward

By stepping into these capabilities, you are no longer just writing Python code that connects to a database. Rather, you are building intelligent, integrated systems that can scale, adapt, and evolve. InterSystems IRIS is different by design, and by learning to use its full platform capabilities through Python, you become a different kind of developer: a power user capable of building data-driven applications that move faster and do more.

Your Python journey with InterSystems IRIS does not end here. There are always new and exciting Python InterSystems IRIS applications being created, whether they are official productions de veloped by InterSystems or open-source projects created by members on the OpenExchange. Join the InterSystems Developer Community to immerse yourself in the growing Python developer community and continue learning more by visiting the Online Learning platform and checking out more in documentation.
