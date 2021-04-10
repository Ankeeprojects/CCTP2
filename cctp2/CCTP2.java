/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package cctp2;

import java.io.*;
import java.lang.*;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.SocketTimeoutException;
import java.nio.file.Files;
import java.util.Map;
import java.util.Random;


/**
 *
 * @author pedro
 */

public class CCTP2 {

    /**
     * @param args the command line arguments
     */
    static InetAddress servidor;
    static int porta;
    
    public static Boolean quermeta(String mensagem) {
        return mensagem.substring(0, 7).equals("Getmeta");
    }
    
    public static Boolean querdados(String mensagem) {
        return mensagem.substring(0,5).equals("Chunk");
    }
    
    public static Boolean autentica (DatagramSocket socket) throws IOException {
        byte[] buffer = new byte[1000];
        DatagramPacket pacote = new DatagramPacket(buffer, buffer.length);
        
        byte[] bufi = ("CC21").getBytes();
        socket.send(new DatagramPacket(bufi, bufi.length, servidor, 9999));
        socket.setSoTimeout(2000);
        
        try {
            socket.receive(pacote);

            while (!pacote.getAddress().equals(servidor) || pacote.getPort() != porta) {
                pacote = new DatagramPacket(buffer, buffer.length);
                buffer = new byte[1000];
                socket.receive(pacote);
            }

            if (new String (pacote.getData()).substring(0, 11).equals("Autenticado")) {
                System.out.println("Fui autenticado com a mensagem: " + new String (pacote.getData()) + 
                    " endereço e porta " + pacote.getAddress() + ":" + pacote.getPort());
                return true;
            }
            return false;
        } catch (SocketTimeoutException e){
            System.out.println("Dei exceção, e cá estou com o server " + servidor + ":" + porta);

            return false;
        }
    }
    
    public static void main(String[] args) throws Exception {
        DatagramSocket socket = new DatagramSocket();
        String[] campos;
        DatagramPacket pacote;
        servidor = InetAddress.getByName(args[0]);
        porta = Integer.valueOf(args[1]);
        
        if (autentica(socket)) {
            while (true) {
                byte[] buf = new byte[1500];
                pacote = new DatagramPacket(buf, buf.length);
                socket.setSoTimeout(0);
                socket.receive(pacote);
                InetAddress address = pacote.getAddress();
                int porta = pacote.getPort();
                String mensagem = new String (pacote.getData());
        
                if (quermeta(mensagem)) {
                    campos = mensagem.split(" ");
                    File f = new File (campos[1].substring(1));
                    
                    if(f.exists()) {
                        System.out.println("Ficheiro " + campos[1] + " tem " + f.length() + " bytes e é do tipo " +
                                Files.probeContentType(f.toPath()));
                        buf = ("OK " + f.length() + " " + Files.probeContentType(f.toPath()) + " ").getBytes();
                        pacote = new DatagramPacket(buf, buf.length, address, porta );      
                    } else {
                        buf = ("NOTOK ").getBytes();
                        System.out.println("Tentei abrir " + campos[1] + " mas não deu");
                    }
                    pacote = new DatagramPacket(buf, buf.length, address, porta );
                    socket.send(pacote);
                    
                } else if (querdados(mensagem)) {
                    System.out.println(mensagem);
                    campos = mensagem.split(" ");

                    int quantos;
                    int chunk = Integer.valueOf(campos[1].replaceAll("\\D+","")) * 1024;
                    File f = new File(campos[2]);
                    FileInputStream inputStream = new FileInputStream(f);
                    
                    inputStream.skipNBytes(chunk);
                    if (f.length() > chunk + 1024)
                        quantos = 1024;
                    else quantos = (int) f.length() - chunk;

                    byte[] dados = new byte[quantos];
                    inputStream.read(dados, 0, quantos);
                    inputStream.close();
                    pacote = new DatagramPacket(dados, quantos, address, porta);
                    
                    socket.send(pacote);
                }
           }
        } else {
            System.out.println("Não foi possível autenticar com servidor HttpGw, tente novamente mais tarde.");
        }
    }
}
    
