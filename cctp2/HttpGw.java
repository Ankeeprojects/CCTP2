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
import java.io.FileInputStream;
import java.net.InetSocketAddress;
import java.util.AbstractMap;
import java.util.LinkedList;
import java.util.Map.Entry;
import java.util.Queue;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.ReentrantLock;
import java.net.SocketTimeoutException;
import java.nio.file.Path;
import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.zip.CRC32;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;

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
        byte[] checkSum;
        
        public FileDetails (String nomeFicheiro, int chunks, int tamanho, DataOutputStream out, byte[] checkSum){
            this.nomeFicheiro = nomeFicheiro;
            this.chunkTot = chunks;
            this.tamanho = tamanho;
            this.out = out;
            this.checkSum = checkSum;
        }
    }
    
    public static byte[] longToBytes(long l, int tamanho) {
        byte[] result = new byte[8 + tamanho];
            for (int i = 7; i >= 0; i--) {
                result[i] = (byte)(l & 0xFF);
                l >>= 8;
            }
        return result;
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
        
        public void escreveChunk (byte[] buf, int chunkPiece, int chunkTot) throws IOException {
            if (chunkTot == chunkPiece) {
                System.out.println("Tamanho: " + Integer.toHexString(fileDetails.tamanho) + " chunkpiece: " + quantosChunks.get());
                fileDetails.out.write((Integer.toHexString(fileDetails.tamanho) + "\r\n").getBytes());
                fileDetails.out.write(buf, 8, fileDetails.tamanho);
                fileDetails.out.write(("\r\n".getBytes()));
                fileDetails.out.flush();
                fileDetails.out.write((Integer.toHexString(0) + "\r\n\r\n").getBytes());
                fileDetails.out.flush();
                
            } else {
                System.out.println("Tamanho: " + chunkSize + " chunkpiece: " + quantosChunks.get());
                fileDetails.out.write((Integer.toHexString(chunkSize) + "\r\n").getBytes());
                fileDetails.out.write(buf,8, chunkSize);
                fileDetails.out.write(("\r\n".getBytes()));

                fileDetails.out.flush();
            }
        }
        
        public static byte[] longToBytes(long l, int tamanho) {
            byte[] result = new byte[8 + tamanho];
                for (int i = 7; i >= 0; i--) {
                    result[i] = (byte)(l & 0xFF);
                    l >>= 8;
                }
            return result;
        }
        
        @Override
        public void run () {
            int chunkPiece = 0;
            int reenvio = 0;
            Boolean retry = false;
            try {
                DatagramSocket dataSocket = new DatagramSocket();
                
                dataSocket.setSoTimeout(2000);
                        
                while (retry || (chunkPiece = coiso.getAndIncrement()) - 1  < fileDetails.chunkTot) {
                    try {
                        byte[] buf = ("Chunk " + chunkPiece + " " + fileDetails.nomeFicheiro + " ").getBytes();
                        CRC32 crc = new CRC32();
                        //voltar
                        byte[] conteudo = new byte[buf.length + 8];
                        
                        System.arraycopy(buf, 0, conteudo, 8, buf.length);
                        System.arraycopy(fileDetails.checkSum, 0, conteudo, 0, fileDetails.checkSum.length);
                        crc.update(conteudo, 0 , conteudo.length);
                        
                        //voltar
                        
                        byte[] mensagem = longToBytes(crc.getValue(), conteudo.length);
                        System.arraycopy(conteudo, 0, mensagem, 8, conteudo.length);
                        DatagramPacket pacote = new DatagramPacket(mensagem, mensagem.length, servidor, porta);
                        //checkSum 
                        //pede os dados desse chunk
                        dataSocket.send(pacote);
                        
                        buf = new byte[1500];
                        
                        pacote = new DatagramPacket(buf, buf.length);
                        dataSocket.receive(pacote);
                        
                        while (!pacote.getAddress().equals(servidor) || pacote.getPort() != porta) {
                            buf = new byte[1500];
                            pacote = new DatagramPacket(buf, buf.length, servidor, porta);
                            dataSocket.receive(pacote);
                        }
                        
                        if (checkSum(pacote.getData(), pacote.getLength())) {
                            try {
                                stackDetails.fStackLock.lock();    

                                while (quantosChunks.get() != chunkPiece)
                                    stackDetails.fullStack.await();

                                escreveChunk(buf, fileDetails.chunkTot, chunkPiece);  
                                quantosChunks.getAndIncrement();
                                stackDetails.fullStack.signalAll();
                                retry = false;
                                reenvio = 0;
                            } catch (InterruptedException | IOException e) {
                            } finally {
                                stackDetails.fStackLock.unlock();
                            }
                        } else {
                            System.out.println("checksum error");
                            if (++reenvio > 2) {
                               System.out.println("O checksum falhou 3 vezes, eliminando o servidor.");
                               reenvio = 0;
                               Entry<InetAddress,Integer> serv = procuraServidor();
                               servidor = serv.getKey();
                               porta = serv.getValue();
                            }
                            retry = true;
                        }
                    } catch (SocketTimeoutException e){
                        System.out.println("Dei exceção, e cá estou com o server " + servidor + ":" + porta);
                        Entry<InetAddress,Integer> serv = procuraServidor();
                        servidor = serv.getKey();
                        porta = serv.getValue();
                        
                        retry = true;
                        reenvio = 0;
                    }
                }
            } catch (IOException e) {
            }
            devolveServidor();
        }
    }
    
    public static boolean checkSum (byte[] pacote, int tamanho) {
        CRC32 crc = new CRC32();
        
        crc.update(pacote,8,tamanho - 8);
        long crcOriginal = bytesToLong(pacote,0);
        
        System.out.println("Long: " + crcOriginal + " checksum: " + crc.getValue());
        return Long.compare(crc.getValue(), crcOriginal) == 0;
    }
    
    public static long bytesToLong(byte[] b, int offset) {
        long result = 0;
        for (int i = offset; i < offset+8; i++) {
            result <<= 8;
            result |= (b[i] & 0xFF);
        }
        return result;
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
        
        public Boolean validaCodigo (String codigo) {
            return codigo.substring(8, 12).equals("CC21");
        }
        
        public void enviaAuth (InetAddress address, int porta) throws IOException{
            Map.Entry<InetAddress, Integer> t = new AbstractMap.SimpleEntry<>(address, porta);
            byte[] buf;
            DatagramPacket pacote;
            CRC32 crc = new CRC32();
                
            try {
                lock.lock();
                servidores.add(t);
                buf = ("Autenticado").getBytes();
                crc.update(buf, 0, buf.length);
                byte[] buff = longToBytes(crc.getValue(), buf.length);
                System.arraycopy(buf, 0, buff, 8, buf.length);
                pacote = new DatagramPacket(buff, buff.length, address, porta);
                
                socket.send(pacote);
                c.signalAll();
            } finally {
                lock.unlock();
            } 
        }
        
        public void pedeReenvio (InetAddress address, int porta) throws IOException{
            DatagramPacket pacote;
            
            CRC32 crc = new CRC32();
            byte[] bufi = ("Reenvia").getBytes();
            
            crc.update(bufi, 0, bufi.length);
            byte[] buff = longToBytes(crc.getValue(), bufi.length);
            System.arraycopy(bufi, 0, buff, 8, bufi.length);

            pacote = new DatagramPacket(buff, buff.length, address, porta);
            socket.send(pacote);
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
                    
                    if (checkSum(pacote.getData(), pacote.getLength())){
                        if (validaCodigo(recebido))
                            enviaAuth(address, porta);
                        else 
                            System.out.println("Não validado com pass " + recebido);
                    } else {
                        pedeReenvio(address, porta);
                    }
                    
                    System.out.println("\nLista de servidores:");
                    for (Map.Entry<InetAddress, Integer> entrada : servidores)
                            System.out.println("Servidor: " + entrada.getKey() + " " + entrada.getValue());
                    
                }
            } catch (Exception e) {}
        }
    }
    
    static public Boolean validaFich (byte[] pacote) {
        
        return new String (pacote, 16, 2).equals("OK");
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
        
        public void contactaFFS (int chunk, FileDetails f, StackDetails s) {
            int servers = 0;
            int tentativas = 0;
            AtomicInteger coiso = new AtomicInteger(0);
            AtomicInteger quantosChunks = new AtomicInteger(0);
            Map.Entry<InetAddress, Integer> serv = null;
            
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
            } catch (Exception e) {
            } finally {
                serverLock.unlock();
            }
        }
        
        public void processaPedido (String mensagem, byte[] checkSum, DataOutputStream out, String nomeFicheiro) throws IOException {
            int chunk = 0;
            String[] campos = mensagem.split(" ");
            int num = Integer.valueOf(campos[1]);
            String tipoFich = campos[2];
            
            ReentrantLock fStackLock = new ReentrantLock();
            Condition fStackCondition = fStackLock.newCondition();

            Integer tamanho = Integer.valueOf(num);
            
            while (tamanho > chunkSize) {
                chunk++;
                tamanho -= chunkSize;
            }

            StackDetails s = new StackDetails (serverLock, fStackLock, fStackCondition, c);
            FileDetails f = new FileDetails(nomeFicheiro, chunk, tamanho, out, checkSum);
            
            contactaFFS(chunk, f, s);
            
            out.write("HTTP/1.1 200 OK\r\n".getBytes());
            out.write(("Content-Type: " + tipoFich + "\r\n").getBytes());
            out.write(("Transfer-Encoding: chunked\r\n\r\n").getBytes());                       
            out.flush();   
        }
        
        public void enviaErro (DataOutputStream out) throws IOException {
            out.write(("HTTP/1.1 404 Not Found\r\n").getBytes());
            out.write(("Connection: close\r\n\r\n").getBytes());
            out.flush();
            out.close();
            System.out.println("ERRO! O ficheiro não existe.");
        }
        
        public void run() {
            String pedido;
               try {
                    System.out.println("Ligado a " + socket.getInetAddress() + ":" + socket.getPort());
                   
                    BufferedReader br = new BufferedReader(new InputStreamReader(socket.getInputStream()));
                
                while ((pedido = br.readLine()) != null) {
                    DataOutputStream out = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream()));

                    Boolean retry = false;
                    Map.Entry<InetAddress, Integer> serv = null;
                    DatagramPacket pacote = null;
                    this.dataSocket = new DatagramSocket();

                    System.out.println("Recebi esta mensagem: " + pedido);
                    String[] parametros = pedido.split(" ");
                    int reenvio = 0;
                    String nomeFicheiro = new String(parametros[1].substring(1));
                    byte[] buf = ("Getmeta" + pedido).getBytes();
                    CRC32 crc = new CRC32();
                    
                    crc.update(buf, 0, buf.length);
                    byte[] resposta = longToBytes(crc.getValue(), buf.length);
                    System.arraycopy(buf, 0, resposta, 8, buf.length);
                    
                    do {
                        //Servidor é encontrado e colocado em serv
                        if (reenvio == 0) {
                            serv = procuraServidor();
                        }

                        pacote = new DatagramPacket(resposta, resposta.length, serv.getKey(), serv.getValue());
                        //Pedido de metadados ao servidor
                        try {
                            dataSocket.send(pacote);
                            dataSocket.setSoTimeout(2000);
                            byte[] buff = new byte[1500];
                            pacote = new DatagramPacket(buff, buff.length);
                            dataSocket.receive(pacote);

                            if (checkSum(pacote.getData(), pacote.getLength())) {
                                servidores.add(serv);
                                retry = false;
                                reenvio = 0;
                            } else {
                                System.out.println("checksum error");
                                if (++reenvio > 2) {
                                    System.out.println("O checksum falhou 3 vezes, eliminando o servidor.");
                                    reenvio = 0;
                                }
                                retry = true;
                            }
                        } catch (SocketTimeoutException e) {
                            System.out.println("Esta cena deu exceção no " + serv.getKey() + ":" + serv.getValue());
                            retry = true;
                            reenvio = 0;
                        }
                    } while (retry || !pacote.getAddress().equals(serv.getKey()) || pacote.getPort() != serv.getValue());

                    String mensagem = new String(pacote.getData());
                    //System.out.println("Mensagem dos meta " + mensagem);
                    // Metadados recebidos, valida o ficheiro
                    //System.out.println("checkSum do pacote: " + bytesToLong(pacote.getData(), 0));
                    if (validaFich(pacote.getData())) {
                        processaPedido(mensagem, Arrays.copyOfRange(pacote.getData(), 8, 16),out, nomeFicheiro);
                    } else {
                        enviaErro(out);
                    }

                    while (!(br.readLine()).equals(""));
               }
            } catch (IOException e) {}
        }
    }
    
    static class PedidosHTTP implements Runnable {
        ServerSocket serverSock;
        Socket socket;
        ReentrantLock serverLock;
        Condition c;
        
        public PedidosHTTP (ServerSocket serverSock, ReentrantLock serverLock, Condition c) {
            this.serverSock = serverSock;
            this.serverLock = serverLock;
            this.c = c;
        }
        
        
        public void run () {
            try {
                while (true) {
                    socket = serverSock.accept();
                    Thread worker = new Thread(new ChunkWorker(socket, serverLock, c));
                    worker.start();
                }
            } catch (IOException e){}
        }  
    }

    private static ServerSocket getServerSocket(InetSocketAddress address) throws Exception {
        var keyStorePath = Path.of("./keystore.jks");
        char[] keyStorePassword = "pass_for_self_signed_cert".toCharArray();

        ServerSocket serverSocket = getSslContext(keyStorePath, keyStorePassword)
                .getServerSocketFactory()
                .createServerSocket(address.getPort(), 0, address.getAddress());

        return serverSocket;
    }

    private static SSLContext getSslContext(Path keyStorePath, char[] keyStorePass) throws Exception {

        KeyStore keyStore = KeyStore.getInstance("JKS");
        keyStore.load(new FileInputStream(keyStorePath.toFile()), keyStorePass);

        KeyManagerFactory keyManagerFactory = KeyManagerFactory.getInstance("SunX509");
        keyManagerFactory.init(keyStore, keyStorePass);

        SSLContext sslContext = SSLContext.getInstance("TLS");

        // TrustManager e SecureRandom a null para serem as implementacoes default destes
        sslContext.init(keyManagerFactory.getKeyManagers(), null, null);
        return sslContext;
    }

    public static void main (String[] args) throws SocketException, UnknownHostException, IOException, NoSuchAlgorithmException, KeyManagementException, Exception {
        ServerSocket httpSock = new ServerSocket(8080);
        ServerSocket httpsSock = getServerSocket(new InetSocketAddress("localhost", 8181));
        DatagramSocket s = new DatagramSocket(9999, InetAddress.getByName("localhost"));
        
        ReentrantLock lock = new ReentrantLock();
        Condition condition = lock.newCondition();
        
        Thread t = new Thread(new Authenticator(s, lock, condition));
        t.start();
        
        Thread http = new Thread(new PedidosHTTP(httpSock, lock, condition));
        http.start();
        
        Thread https = new Thread(new PedidosHTTP(httpsSock, lock, condition));
        https.start();
    }
}

