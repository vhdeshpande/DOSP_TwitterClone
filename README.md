# Project 4: Twitter Engine Clone

### Authors:
* Vaibhavi Deshpande
* Ishan Kunkolikar
### Pre-requisites:
* Erlang/OTP version - 25.1
* Cowboy version – 2.9
### Steps to run:
* Commands to start the algorithm:
``` 
c( twitter_engine_server_utils ).
c( twitter_engine_clients_utils ).
c( twitter_engine_simulator ).
twitter_engine_server_utils:initialise_gen_server_link().
twitter_engine_client_utils:register_user ( user ).
twitter_engine_start_simulator().
```
Where ‘user’ is the registering user.

* Dependencies added to build: `DEP_PLUGINS = cowboy`
* Build using `Erlang.mk`
* Commands to start the program:
``` 
Make run
Run on http://localhost:8080/
``` 

### Implementation details:
In the project scope, the users can send a tweet, retweet another user's tweet, subscribe to another user, use a hashtag in a tweet, and mention other users in the tweet. Twitter engine simulator simulates the Twitter environment for testing the scalability of the system by simulating multiple users who are registering, subscribing, tweeting, and live connection and disconnection for the user. The Twitter engine clone consists of three components: `twitter_engine_client_utils`, `twitter_engine_server_utils`, and `twitter_engine_simulator`. The `twitter_engine_client_utils` supports the functionalities to be used and invoked by the user like register, subscribe, tweet, and retweet which are supported and maintained in the `twitter_engine_server_utils`.
Modules Utilized :
* **Gen server**: Gen server is implemented for a more seamless client-server interaction. The supervision tree helps keep all the processes linked. This module is used to build processes that can handle requests from the client to the server.
* **ETS**: This module was utilized to store data as tuples. Every lookup and insert then produces a copy of the object. All the updates and inserts to the table are atomic and isolated.
### Simulator:
The simulator replicates the number of users that are being registered. The simulator is given the number of users and a number of subscribers. According to this input, the simulator will run the register, tweet, and subscribe functionalities for all the users and subscribers according to the Zipf law.
### **Zipf law implementation:**
* The frequencies of subscribers are inversely proportional to the subsequent users.
* The users are simulated as numbers from 1 to n, n being total users.
* Thus the subscribers will be part of this set according to the following formula provided the
number of subscribers does not exceed n-1.
This is being done with an index value and the total subscribers are divided by this value. The subscribe function in the client utils is being called on all the subscribers according to the Zipf law.
### Architecture Details:
* **Register:** Users can use register themselves using this method. This information is then sent to the server side where the user information is inserted in an ETS table.
* **Login:** A user can use login if they have logged out or have recently registered. This data is sent to the server where a table stores the logged-in and logged-out as atoms true or false.
* Subscribe: This method takes two arguments. A user can follow any other user. This data is stored in a table in the form of a tuple in two tables. The first table stores { subscribes } and the second stores { subscribed to }. This makes it easier to retrieve data of followers as well as a list of whom the user follows.
* **Tweet:** After a user is specified the user can input a tweet in the form of a string. This data is then stored in a table of all tweets as a tuple { user, tweet }. This tweet is also parsed for any Mentions and Hashtags. If any Hashtags are found then the tweet along with the tag is stored in the table that can be looked up with the hashtag value. If any mentions are found then the tweet is stored with the mentioned user name and this table can later be looked up by the User name.
* **Timeline:** This method is used to retrieve all tweets and the retweets that a user has got from the people that they follow. This timeline will consist of tweets stored in the table in which we have stored tweets from
* **Retweet:** All tweets are associated with a unique identifier. A user can browse through all tweets of another user and can choose to retweet any tweet they can using the tweet’s unique identifier. This unique identifier is then used to retrieve the tweet and it is stored under the current user in the retweet table.
* **Search by Hashtag:** The user enters the tag with which they want to parse the set of all tweets. It then looks up the table where all tweets are stored with the index of the tag and returns the complete list of such tweets.
* **Search by Mentions:** The user enters the mentions that they want to parse the complete set of tweets with. Then in the mentions table, we look up all tweets with the parsed value of the mentioned user name.
* **Log out:** The user can log out by using the logout method. We will flip the variable in the table which stores the current status of the user. The user will then need to log in before using any of the functionality.

### Websocket Implementation
The aim of the project is to implement a Twitter engine clone integrating a WebSocket interface with the Part 1 project enabling the client-server implementation. The project uses the Cowboy WebSocket framework to integrate the client with the server where the WebSocket connection is supported by the Cowboy WebSocket handlers.
### Functionality:
The functionality supported includes – registering users, login in, sending tweets, retweeting, subscribing to a user, searching using hashtags, and searching using mentions.
Supported Interfaces:
The interfaces for the following functions are defined in the module websocket_app:
* `/register` – The interface enables a new user to register, it creates and stores the user
entry into the user_table_list
* `/login` – The interface validates if the user is registered with user_table_list
* `/tweet` – The interface is used for posting a tweet
* `/retweet` – The interface is used for supporting retweet
* `/subscribe` – The interface supports following/subscribing to another user functionality
* `/tweetview` – The interface fetches tweet feeds for the user based on the user’s
subscription preferences
* `/search` – The interface supports the search functionality based on hashtags and mentions

### Technologies Used:
* **Front-end:** The front-end uses JavaScript framework utilizing the Cowboy framework to support WebSocket connection with Erlang through the defined handlers
* **Back-end:** The back-end utilizes Erlang with interfaces supported in the component `twitter_engine_client_utils` and maintained in the component `twitter_engine_server_utils` using the Gen Server and ETS modules.
