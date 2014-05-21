/*
* Author:  Filip Dvořák <filip.dvorak@runbox.com>
*
* Copyright (c) 2013 Filip Dvořák <filip.dvorak@runbox.com>, all rights reserved
*
* Publishing, providing further or using this program is prohibited
* without previous written permission of the author. Publishing or providing
* further the contents of this file is prohibited without previous written
* permission of the author.
*/
package fape.core.execution;

import fape.util.TinyLogger;
import java.io.*;
import java.net.Socket;

/**
*
* @author FD
*/
public class Listener {

    // The host where the message passer is running.
    // sane default would be localhost
    private final String OPRS_HOST;
    // The name of the OPRS Kernel you want to talk to. Note that you
    // can talk to any program connected to the message passer, his is
    // just an example.
    // sane default would be OPRS
    private final String OPRS_MANIP;
    // Your name (this is where other program will send you message).
    private final String CLIENT_NAME;
    // The default port to connect to the message passer.
    // sane default would be 3300
    private final int SOCKET_MP;

    private boolean debug = false;
    private PrintWriter ostream;
    private OutputStream out;
    private BufferedReader istream;
    private ListenMessagePasser lmp;
    private String sender;
    // return states
    public static final int ERROR = -1;
    public static final int OK = 1;

    /** Executor to which the messages are forwarded */
    private ExecutorPRS exec;

    /** Socket connect to the message passer */
    Socket socket = null;

    /**
     *
     * @param e
     */
    public void bind(ExecutorPRS e) {
        exec = e;
    }

    /**
     * Closes the socket which causes the thread to stop.
     */
    public void abort() {
        assert socket != null : "Socket doesn't exists.";
        try {
            socket.close();
        } catch (IOException e) {
            System.err.println("Error while closing the socket: "+ e);
        }
    }

    /**
     * "name of the machine", "who am I talking to", "my name (fape)", "3300"
     *
     * @param oprs_host Host on which the message passer is running.
     * @param oprs_manip Name of OPRS kernel you want to talk to.
     * @param client_name Name that should by given to the message passer for yourself.
     * @param socket_port Port on which the message passer is running.
     */
    public Listener(String oprs_host, String oprs_manip, String client_name, int socket_port) {
        this.OPRS_HOST = oprs_host;
        this.OPRS_MANIP = oprs_manip;
        this.CLIENT_NAME = client_name;
        this.SOCKET_MP = socket_port;
        connect();
    }

    /**
     * To write an int to MP
     *
     * @param i The integer to send to the MP. I am not sure this code is endian
     * robust... may need some testing or tweaking. AFAICT, It works on x86
     * machine.
     */
    private void write_int(int i) {
        byte buffer[] = new byte[4]; // 4 is the length of an int in Java
        buffer[3] = (byte) ((i) & 0xff);
        buffer[2] = (byte) ((i >> 8) & 0xff);
        buffer[1] = (byte) ((i >> 16) & 0xff);
        buffer[0] = (byte) ((i >> 24) & 0xff);
        try {
            out.write(buffer);
            if (debug) {
                System.out.println("Int sent to MP : " + buffer);
            }
        } catch (IOException e) {
            System.err.println("MessagePasserClient->write_int :\nException : " + e + "Couldn't write on the socket");
        }
    }

    /**
     * To send a string to MP
     *
     * @param s The string to send to MP
     */
    private void write_string(String s) {
        int length;
        length = s.length();
        byte buffer[] = new byte[4 + length]; // 4 is the length of an int in Java
        buffer[3] = (byte) ((length) & 0xff);
        buffer[2] = (byte) ((length >> 8) & 0xff);
        buffer[1] = (byte) ((length >> 16) & 0xff);
        buffer[0] = (byte) ((length >> 24) & 0xff);
        int i = 0;
        while (i < length) {
            buffer[i + 4] = (byte) s.charAt(i);
            i++;
        }
        //	buffer[i+4]='\0';
        try {
            out.write(buffer);
            if (debug) {
                System.out.println("String sent to MP : " + buffer);
            }
        } catch (IOException e) {
            System.err.println("MessagePasserClient->write_string :\nException : " + e + "Couldn't write on the socket");
        }
    }

    private void connect() {
        /* Connection */
        try {
            /* Create the sockets */
            System.out.print("Connecting socket to Message Passer...");
            socket = new Socket(OPRS_HOST, SOCKET_MP);
            System.out.println(" done.");
            out = socket.getOutputStream();
            ostream = new PrintWriter(out);
            InputStream in = socket.getInputStream();
            InputStreamReader reader = new InputStreamReader(in);
            istream = new BufferedReader(reader);
        } catch (IOException e) {
            System.err.println(" FAILED.");
            System.err.println("MessagePasserClient->initConnection :\nError :" + e + "\nCould not connect to Message Passer...");
            throw new RuntimeException("Could not connect to message passer on host:port: "+OPRS_HOST+":"+SOCKET_MP);
        }

        /* Now that we are connected, we'll send the protocol and client name to the message passer */
        try {
            System.out.print("Sending client infos to Message Passer...");
            write_int(1); // The protocol, corresponding to STRINGS_PT
            write_string(CLIENT_NAME); // We send our name for identification.
            System.out.println(" done.");
        } catch (Exception e) {
            System.out.println(" FAILED");
            System.out.println("MessagePasserClient->initConnection :\nError :" + e + "\nCould not write on the socket...");
            throw new UnsupportedOperationException();
        }

        /* And to end this initialisation, we will launch a thread that will listen for messages */
        lmp = new ListenMessagePasser();
        // don't keep the VM awake if no other threads are running
        lmp.setDaemon(true);
        lmp.start();
        System.out.println("Now listening to the Message Passer on socket : " + SOCKET_MP);
    }

    /**
     * The class that will be listening on the socket
     */
    private class ListenMessagePasser extends Thread {

        public ListenMessagePasser() {
        }

        public int ReadInt() throws IOException {
            int b1 = istream.read();
            int b2 = istream.read();
            int b3 = istream.read();
            int b4 = istream.read();

            int res;
            res = (b1 << 24) + (b2 << 16) + (b3 << 8) + b4;

            return res;
        }

        /**
         * The code executed when the thread is started.
         */
        @Override
        public void run() {
            try {
                // At the very beginning, we receive four 0, so we get rid of them
                // They correspond to the REGISTER_OK

                int protocol = ReadInt();
                // In fact, we should check that we are getting REGISTER_OK, otherwise something went wrong.

                /* Then we are ready to receive a message
                 * As we are in STRINGS_PT mode, a message will look like :
                 * an int giving the length of the string for the sender, the sender's name (ascii),
                 * an int for the length of the message, and the message itself (ascii).
                 */
                while (true) {
                    int froms = ReadInt();
                    char[] from = new char[froms];
                    istream.read(from, 0, froms);

                    int messages = ReadInt();
                    char[] message = new char[messages];
                    istream.read(message, 0, messages);

                    receivedMessage(new String(from), new String(message));
                }
            } catch (java.net.SocketException closed) {
                // socket closed, graceful stop
                System.out.println("[FAPE] Closing connection to message passer.");
            } catch (IOException e) {
                System.out.println("Error :" + e + "\nError while listening on the socket...");
            }
        }
    }

    public void SimulateMessageReceived(String msg){
        receivedMessage("", msg);
    }

    private void receivedMessage(String from, String message) {
        message = message.replaceAll("\\\\n", "\n");
        TinyLogger.LogInfo("Message received: " + message);

        // forward message to executor
        exec.eventReceived(message);
    }

    /**
     * Send a message to the supervisor. The message is encapsulated into the
     * protocol defined by Message Passer.
     *
     * @param msg The message to send
     */
    public int sendMessage(String msg) {
        TinyLogger.LogInfo("Sending message: " + msg);
        /* The format of a message is :
         MessageType+sizeof(DestinationName)+DestinationName+sizeof(msg)+msg*/
        // This code works, but I am not sure why it is not using write_int and write_string above...
        // Moreover, to be more generic, it should take two argument, the recipient and the msg.
        int message_mt = 1; // MESSAGE_MT
        int lengthdest = OPRS_MANIP.length();
        int lengthmsg = msg.length();
        int size = 4 + 4 + lengthdest + 4 + lengthmsg;
        byte buffer[] = new byte[size];
        // Write the message byte per byte
        // Message type
        buffer[0] = (byte) ((message_mt >> 24) & 0xff);
        buffer[1] = (byte) ((message_mt >> 16) & 0xff);
        buffer[2] = (byte) ((message_mt >> 8) & 0xff);
        buffer[3] = (byte) ((message_mt) & 0xff);
        // Length of the recipient's name
        buffer[4] = (byte) ((lengthdest >> 24) & 0xff);
        buffer[5] = (byte) ((lengthdest >> 16) & 0xff);
        buffer[6] = (byte) ((lengthdest >> 8) & 0xff);
        buffer[7] = (byte) ((lengthdest) & 0xff);
        // The recipient's name
        int i = 0;
        while (i < lengthdest) {
            buffer[i + 8] = (byte) OPRS_MANIP.charAt(i);
            i++;
        }
        // The length of the message
        buffer[8 + i] = (byte) ((lengthmsg >> 24) & 0xff);
        buffer[9 + i] = (byte) ((lengthmsg >> 16) & 0xff);
        buffer[10 + i] = (byte) ((lengthmsg >> 8) & 0xff);
        buffer[11 + i] = (byte) ((lengthmsg) & 0xff);
        // The message
        int j = 0;
        while (j < lengthmsg) {
            buffer[12 + i + j] = (byte) msg.charAt(j);
            j++;
        }

        // Send the message
        try {
            out.write(buffer);
            if (debug) {
                System.out.println("Message sent to supervisor : " + buffer);
            }
        } catch (IOException e) {
            System.err.println("MessagePasserClient->sendMessageToSupervisor :\nException : " + e + "Couldn't write on the socket");
            return ERROR;
        }
        return OK;
    }

}
