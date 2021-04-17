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
import java.util.zip.CRC32;


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
    
    public static Boolean quermeta(byte[] mensagem) {
        return new String(mensagem, 8,7).equals("Getmeta");
    }
    
    public static Boolean querdados(byte[] mensagem) {
        return new String(mensagem, 16, 5).equals("Chunk");
    }
    
    public static byte[] longToBytes(long l, int tamanho) {
        byte[] result = new byte[8 + tamanho];
            for (int i = 7; i >= 0; i--) {
                result[i] = (byte)(l & 0xFF);
                l >>= 8;
            }
        return result;
    }
    
    
    public static long bytesToLong(byte[] b) {
        long result = 0;
        for (int i = 0; i < 8; i++) {
            result <<= 8;
            result |= (b[i] & 0xFF);
        }
        return result;
    }
    
    public static boolean checkSum (byte[] pacote, int tamanho) {
        CRC32 crc = new CRC32();
        
        crc.update(pacote,8,tamanho - 8);
        long crcOriginal = bytesToLong(pacote);
        
        System.out.println("Long: " + crcOriginal + " checksum: " + crc.getValue());
        return Long.compare(crc.getValue(), crcOriginal) == 0;
    }
    
    public static Boolean autentica (DatagramSocket socket) throws IOException {
        byte[] buffer = new byte[1000];
        DatagramPacket pacote = new DatagramPacket(buffer, buffer.length);
        
        byte[] bufi = ("CC21").getBytes();
        CRC32 crc = new CRC32();
        crc.update(bufi, 0, bufi.length);
        byte[] buff = longToBytes(crc.getValue(), bufi.length);
        System.arraycopy(bufi, 0, buff, 8, bufi.length);
        System.out.println(new String(buff) + "tamanho: " + bufi.length);
        socket.send(new DatagramPacket(buff, buff.length, servidor, 9999));
        socket.setSoTimeout(2000);
        
        try {
            socket.receive(pacote);

            while (!pacote.getAddress().equals(servidor) || pacote.getPort() != porta) {
                pacote = new DatagramPacket(buffer, buffer.length);
                buffer = new byte[1000];
                socket.receive(pacote);
            }
            
            if (checkSum(pacote.getData(), pacote.getLength())) {
                System.out.println("Recebi " + new String(pacote.getData()));
                String s = new String(pacote.getData(), 8, 11);
                
                if (s.equals("Autenticado")) {
                    System.out.println("Fui autenticado com a mensagem: " + new String (pacote.getData()) + 
                        " endereço e porta " + pacote.getAddress() + ":" + pacote.getPort());
                    return true;
                }
            } else {
                System.out.println("tamanho:" + pacote.getLength() + " " + new String(pacote.getData()));
                System.out.println("Checksum errado no pacote de resposta à autenticação");
            }
            
            return false;
        } catch (SocketTimeoutException e){
            System.out.println("Timeout, não recebi resposta em tempo útil.");

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
                
                //TODO verificar checksum e transformar em array de bytes antes..
                socket.setSoTimeout(0);
                socket.receive(pacote);
                InetAddress address = pacote.getAddress();
                int port = pacote.getPort();
                
                if (address.equals(servidor)) {
                    byte[] mensagem = pacote.getData();
                    if (quermeta(mensagem)) {
                        campos = new String(mensagem, 8, mensagem.length-8).split(" ");
                        File f = new File (campos[1].substring(1));
                        Long fileCheckSum;
                        byte[] conteudo;
                        /*Random r = new Random();
                        if (r.nextBoolean())
                            try {
                                Thread.sleep(1233424);
                            } catch (Exception e) {}
                        */

                        if(f.exists()) {
                            System.out.println("Ficheiro " + campos[1] + " tem " + f.length() + " bytes e é do tipo " +
                                    Files.probeContentType(f.toPath()));
                            byte[] buffz = Files.readAllBytes(f.toPath());
                            CRC32 crc = new CRC32();
                            crc.update(buffz, 0, buffz.length);
                            buf = ("OK " + f.length() + " " + Files.probeContentType(f.toPath()) + " ").getBytes();
                            byte[] msgAux = longToBytes(crc.getValue(), 0);
                            conteudo = new byte[buf.length + 8];
                            System.out.println("O checkSum do ficheiro é " + crc.getValue());
                            System.arraycopy(buf, 0, conteudo, 8, buf.length);
                            System.arraycopy(msgAux, 0, conteudo, 0, msgAux.length);
                            //System.out.println("O conteudo da cena é " + new String(conteudo));
                        } else {
                            conteudo = ("ERRORFILEISNOTAVAILABLE ").getBytes();
                            System.out.println("Tentei abrir " + campos[1] + " mas não deu");
                        }

                        CRC32 crc = new CRC32();
                        crc.update(conteudo, 0, conteudo.length);
                        byte[] buff = longToBytes(crc.getValue(), conteudo.length);

                        System.arraycopy(conteudo, 0, buff, 8, conteudo.length);

                        pacote = new DatagramPacket(buff, buff.length, address, port);
                        socket.send(pacote);

                    } else if (querdados(pacote.getData())) {
                        System.out.println("quer dados: " + mensagem);
                        
                        campos = new String(mensagem, 16, mensagem.length-16).split(" ");

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

                        CRC32 crc = new CRC32();
                        crc.update(dados, 0, dados.length);
                        byte[] buff = longToBytes(crc.getValue(), dados.length);

                        System.arraycopy(dados, 0, buff, 8, dados.length);

                        pacote = new DatagramPacket(buff, buff.length, address, port);

                        socket.send(pacote);
                    }
                } else {
                    System.out.println("bugou " + servidor + " " + pacote.getAddress() + porta + " " + port);
                }
           }
        } else {
            System.out.println("Não foi possível autenticar com servidor HttpGw, tente novamente mais tarde.");
        }
    }
}
    
