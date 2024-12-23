# What is Rooms?

Rooms is an editor-based social networking service for engineers that makes programming a more fun, exciting and connected experience.  
It provides a pair programming environment where you can share your coding scenes, share your skills and interact with other great engineers.  
It delivers an experience where you don't just write code, but create, learn and inspire each other, That's what Rooms.

Would you like to try it?  
Then, I'll introduce how to experience Rooms with [Lem](https://github.com/lem-project/lem) as the client.

## Install

### Docker
As long as you have docker, you can try out Rooms even if you don't have Lem installed.

```
$ make docker
$ make run
```

After this, open localhost:50000 from your browser.

### Use from your lem

You will need nodejs and the latest version of Lem.

```
$ git clone https://github.com/lem-cloud/lem-rooms-client
```

Write the setting to load lem-rooms-client in `$HOME/.lem/init.lisp`

```lisp
(asdf:load-asd (probe-file "<lem-rooms-client directory>/lem-rooms-client.asd"))
(ql:quickload :lem-rooms-client)
```

## Usage

### sign in
You must sign in first to use it.
GitHub OAuth is used.

```
M-x rooms-sign-in
```

### Create a room

First, create a room.
A room is a workspace associated with a directory on the room owner's machine.

```
M-x rooms-create-room
```

### List of rooms and joining

You can look a list of rooms created by other users.
If you select a room, you can enter it and collaborate on a project.

```
M-x rooms-list
```

### Invitation to a private room

If you select ‘private’ when creating a room, you can create a private room that will not appear in the room list.

There is a command to invite other users to the room.
An invitation code will be issued, so please share it with the users you want to invite.

```
M-x rooms-publish-invitation
```

Users who have received an invitation can enter the private room using the invitation code.

```
M-x rooms-join-by-invitation-code
```
