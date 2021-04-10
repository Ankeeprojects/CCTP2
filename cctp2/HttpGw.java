/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cctp2;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.net.UnknownHostException;
import java.util.Map;
import java.io.BufferedOutputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.nio.file.Files;
import java.util.AbstractMap;
import java.util.LinkedList;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;
import java.net.SocketTimeoutException;

/**
 *
 * @author pedro
 */

public class HttpGw {
    static int chunkSize = 1024;
    static Queue<Map.Entry<InetAddress, Integer>> servidores = new LinkedList<>();
    
    static class StackDetails {
        ReentrantLock fStackLock;
        ReentrantLock serverLock;
        Condition fullStack;
        Condition serverC;
        
        public StackDetails (ReentrantLock serverLock, ReentrantLock fStackLock, Condition fullStack, Condition serverC) {
            this.serverLock = serverLock;
            this.fullStack = fullStack;
            this.fStackLock = fStackLock;
            this.serverC = serverC;
        }    
    }
    
    static class FileDetails {
        int chunkTot;
        String nomeFicheiro;
        int tamanho;
        DataOutputStream out;
        
        public FileDetails (String nomeFicheiro, int chunks, int tamanho, DataOutputStream out){
            this.nomeFicheiro = nomeFicheiro;
            this.chunkTot = chunks;
            this.tamanho = tamanho;
            this.out = out;
        }
    }
    
    static class ChunkGetter implements Runnable {
        DatagramSocket socket;
        InetAddress servidor;
        int porta;
        FileDetails fileDetails;
        StackDetails stackDetails;
        AtomicInteger coiso;
        AtomicInteger quantosChunks;
        
        public ChunkGetter (Entry<InetAddress, Integer> e, FileDetails fileDetails, StackDetails stackDetails, 
                AtomicInteger coiso, AtomicInteger quantosChunks) throws SocketException {
            this.socket = new DatagramSocket();        
            this.servidor = e.getKey();
            this.porta = e.getValue();
            this.fileDetails = fileDetails;
            this.stackDetails = stackDetails;
            this.coiso = coiso;
            this.quantosChunks = quantosChunks;
        }
        
        public void devolveServidor() {
            try {
                stackDetails.serverLock.lock();
                servidores.add(new AbstractMap.SimpleEntry<InetAddress, Integer>(servidor, porta));
                stackDetails.serverC.signalAll();
            } finally {
                stackDetails.serverLock.unlock();
            }
        }
        
        
        public Map.Entry<InetAddress, Integer> procuraServidor () {
            Map.Entry<InetAddress, Integer> serv = null;

            try {
                stackDetails.serverLock.lock();
                if (servidores.size() > 0)
                    serv = servidores.iterator().next();
                while (serv == null) {
                    System.out.println("No servers available, sleeping until one is added");
                    stackDetails.serverC.await();

                    if (servidores.size() > 0)
                        serv = servidores.iterator().next();
                }
                servidores.remove(serv);
            } catch (Exception e) {
            } finally {
                stackDetails.serverLock.unlock();
            } 
        
            return serv;
        }
        
        
        public void run () {
            int chunkPiece = 0;
            Boolean retry = false;
            try {
                DatagramSocket dataSocket = new DatagramSocket();
                
                dataSocket.setSoTimeout(12000);
                        
                while (retry || (chunkPiece = coiso.getAndIncrement()) - 1  < fileDetails.chunkTot) {
                    try {
                        // TODO: meter timeout
                
                        byte[] buf = ("Chunk " + chunkPiece + " " + fileDetails.nomeFicheiro + " ").getBytes();
                        DatagramPacket pacote = new DatagramPacket(buf, buf.length, servidor, porta);

                        //pede os dados desse chunk
                        dataSocket.send(pacote);
                        
                        buf = new byte[1500];
                        
                        // TODO adicionar verificação
                        pacote = new DatagramPacket(buf, buf.length);
                        dataSocket.receive(pacote);
                            
                        //System.out.println("Cheguei aqui e nao timei");
                        
                        try {
                        
                            stackDetails.fStackLock.lock();    
                                
                            while  (quantosChunks.get() != chunkPiece) {
                                stackDetails.fullStack.await();
                            }
                            
                            if (fileDetails.chunkTot == chunkPiece) {
                                System.out.println("Tamanho: " + Integer.toHexString(fileDetails.tamanho) + " chunkpiece: " + chunkPiece);
                                fileDetails.out.write((Integer.toHexString(fileDetails.tamanho) + "\r\n").getBytes());
                                fileDetails.out.write(buf, 0, fileDetails.tamanho);
                                fileDetails.out.write(("\r\n".getBytes()));
                                fileDetails.out.flush();
                                fileDetails.out.write((Integer.toHexString(0) + "\r\n\r\n").getBytes());
                                
                                fileDetails.out.flush();
                                fileDetails.out.close();
              
                            } else {
                                
                                System.out.println("Tamanho: " + chunkSize + " chunkpiece: " + chunkPiece);
                                fileDetails.out.write((Integer.toHexString(chunkSize) + "\r\n").getBytes());
                                fileDetails.out.write(buf,0, chunkSize);
                                fileDetails.out.write(("\r\n".getBytes()));
                                
                                fileDetails.out.flush();
                            }
                            
                            quantosChunks.getAndIncrement();
                            System.out.println("Valor do quantos: " + quantosChunks.get());
                            stackDetails.fullStack.signalAll();
                            retry = false;
                        } catch (InterruptedException e) {
                        } finally {
                            stackDetails.fStackLock.unlock();
                        }
                    } catch (SocketTimeoutException e){
                        System.out.println("Dei exceção, e cá estou com o server " + servidor + ":" + porta);
                   
                        Entry<InetAddress,Integer> serv = procuraServidor();
                        servidor = serv.getKey();
                        porta = serv.getValue();
                        retry = true;
                    }
                }
            } catch (IOException e) {
            }
            devolveServidor();
        }
      
    }
    
    static class Authenticator implements Runnable {
        DatagramSocket socket;
        ReentrantLock lock;
        Condition c;
        
        public Authenticator (DatagramSocket sock, ReentrantLock l, Condition c) {
            this.socket = sock;
            this.lock = l;
            this.c = c;
        }
        
        public Boolean valida (String codigo) {
            return codigo.substring(0, 4).equals("CC21");
        }
        
        public void enviaAuth (InetAddress address, int porta) throws IOException{
            Map.Entry<InetAddress, Integer> t = new AbstractMap.SimpleEntry<>(address, porta);
            byte[] buf;
            DatagramPacket pacote;
            
            try {
                lock.lock();
                servidores.add(t);
                buf = ("Autenticado").getBytes();
                pacote = new DatagramPacket(buf, buf.length, address, porta);
                socket.send(pacote);
                c.signalAll();
            } finally {
                lock.unlock();
            } 
        }
        
        public void run() {
            try {
                while (true) {
                    int porta;
                    byte[] buf = new byte[1024];
                    DatagramPacket pacote = new DatagramPacket(buf, buf.length);
                    String recebido;
                    
                    socket.receive(pacote);
                    
                    InetAddress address = pacote.getAddress();
                    porta = pacote.getPort();
                    
                    recebido = new String(pacote.getData(), 0, pacote.getLength());
                    if (valida(recebido))
                        enviaAuth(address, porta);
                    else 
                        System.out.println("Não validado com pass " + recebido);
                    
                    System.out.println("\nLista de servidores:");
                    for (Map.Entry<InetAddress, Integer> entrada : servidores)
                            System.out.println("Servidor: " + entrada.getKey() + " " + entrada.getValue());
                    
                }
            } catch (Exception e) {}
        }
    }
    
    static public Boolean validaFich (String s) {
        return s.substring(0, 2).equals("OK");
    }
    
    
    static class ChunkWorker implements Runnable{
        Socket socket;
        ReentrantLock serverLock;
        DatagramSocket dataSocket;
        Condition c;
        
        public ChunkWorker (Socket sock, ReentrantLock l, Condition c) {
            this.socket = sock;
            this.serverLock = l;
            this.c = c;
        }
        
        public Map.Entry<InetAddress, Integer> procuraServidor () {
            Map.Entry<InetAddress, Integer> serv = null;

            try {
                serverLock.lock();
                if (servidores.size() > 0)
                    serv = servidores.iterator().next();
                while (serv == null) {
                    System.out.println("No servers available, sleeping until one is added");
                    c.await();

                    if (servidores.size() > 0)
                        serv = servidores.iterator().next();
                }
                servidores.remove(serv);
            } catch (Exception e) {
            } finally {
                serverLock.unlock();
            } 
        
            return serv;
        }
        
        public void run() {
            try {
                BufferedReader br = new BufferedReader(new InputStreamReader(socket.getInputStream()));
                DataOutputStream out = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream()));
                 
                Map.Entry<InetAddress, Integer> serv = null;
                DatagramPacket pacote;
                int chunk = 0;
                this.dataSocket = new DatagramSocket();
                String pedido = br.readLine();
                String[] parametros = pedido.split(" ");
                
                String nomeFicheiro = new String (parametros[1].substring(1));
                byte[] buf = ("Getmeta" + pedido).getBytes();
                //TODO timeout aqui também
                
                do {
                    serv = procuraServidor();

                    //Servidor é encontrado e colocado em serv

                    pacote = new DatagramPacket(buf, buf.length, serv.getKey(), serv.getValue());

                    //Pedido de metadados ao servidor
                    dataSocket.send(pacote);

                    byte[] buff = new byte[1500];
                    pacote = new DatagramPacket(buff, buff.length);
                    
                    //TODO adicionar Timeout
                    dataSocket.receive(pacote);

                    servidores.add(serv);
                    
                } while (!pacote.getAddress().equals(serv.getKey()) || pacote.getPort() != serv.getValue());
                
                String mensagem = new String (pacote.getData());
                
                if (validaFich(mensagem)) {
                    String[] campos = mensagem.split(" ");
                    int num = Integer.valueOf(campos[1]);
                    String tipoFich = campos[2];
                    
                    int servers = 0;
                    int tentativas = 0;
                    AtomicInteger coiso = new AtomicInteger(0);
                    AtomicInteger quantosChunks = new AtomicInteger(0);
                    ReentrantLock fStackLock = new ReentrantLock();
                    Condition fStackCondition = fStackLock.newCondition();
                    
                    Integer tamanho = Integer.valueOf(num);
                    
                    while (tamanho > chunkSize) {
                        chunk++;
                        tamanho -= chunkSize;
                    }
                    
                    StackDetails s = new StackDetails (serverLock, fStackLock, fStackCondition,c);
                    FileDetails f = new FileDetails(nomeFicheiro, chunk, tamanho,out);
                    
                    // TODO: Ir buscar metadados a partir da resposta anterior
                    
                    out.write("HTTP/1.1 200 OK\r\n".getBytes());
                    out.write(("Content-Type: " + tipoFich + "\r\n").getBytes());
                    out.write(("Transfer-Encoding: chunked\r\n\r\n").getBytes());   
                    //out.write(("Connection: keep-alive\r\n\n").getBytes());
                    //out.write(("Content-Length: " + tamanho + "\r\n\r\n").getBytes());                    
                    out.flush();
                    //quantosChunks aumenta quando o chunk realmente é recebido, garante que estão todos no fim.
                    
                        try {
                            serverLock.lock();
                            while (tentativas <= 4 && servers <= chunk && servers < 2 && quantosChunks.get() < chunk+1) {
                                tentativas++;
                                if (servidores.size() > 0) {
                                    serv = procuraServidor();
                                    servers++;

                                    Thread chunkServer = new Thread(new ChunkGetter(serv, f,s, coiso, quantosChunks));
                                    chunkServer.start();
                                }

                                if (servers == 0) {
                                    procuraServidor();
                                    servers++;
                                }
                            }
                        } finally {
                            serverLock.unlock();
                        }  
                         
                } else
                    //TODO RESPOSTA HTTP DE ERRO
                    System.out.println("ERRO! O ficheiro não existe.");
                    
        } catch (IOException e) {}
         
    }
    }
    
    public static void main (String[] args) throws SocketException, UnknownHostException, IOException {
        ServerSocket serverSock = new ServerSocket(8080);
        Socket sock;
        ReentrantLock lock = new ReentrantLock();
        Condition condition = lock.newCondition();
        
        DatagramSocket s = new DatagramSocket(9999, InetAddress.getByName("localhost"));
        Thread t = new Thread(new Authenticator(s, lock, condition));
        t.start();
        
        while (true) {
            sock = serverSock.accept();
            Thread worker = new Thread(new ChunkWorker(sock, lock, condition));
            
            worker.start();
        }
    }
}

