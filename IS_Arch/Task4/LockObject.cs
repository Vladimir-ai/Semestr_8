using System;
using System.Threading;

namespace ConsoleApp1
{   
    public static class Program
    {
        static object locker = new();
        static int x = 0;
        
        public static void Main(string[] args)
        {
            Class1 class1 = new Class1();
            Class2 class2 = new Class2();
            
            Thread class1Thread = new(class1.Run);
            class1Thread.Name = "class1Thread";
            class1Thread.Start();
            
            Thread class2Thread = new(class2.Run);
            class2Thread.Name = "class2Thread";
            class2Thread.Start();
               
        }

        private class Class1
        {
            public void Run()
            {
                lock (locker)
                {
                    for (x = 1; x < 10; x++)
                    {
                        Console.WriteLine($"{Thread.CurrentThread.Name}: {x}");
                        Thread.Sleep(100);
                    }
                }
            }
        }
        
        private class Class2
        {
            public void Run()
            {
                lock (locker)
                {
                    for (x = 5; x > 0; x--)
                    {
                        Console.WriteLine($"{Thread.CurrentThread.Name}: {x}");
                        Thread.Sleep(100);
                    }
                }
            }
        }
    }
}