using System;

namespace ConsoleApp1
{
    public static class Program
    {
        private static void Main(string[] args)
        {
            var caller = new CallbackCaller();
    
            /*
            * Calling method on CallbackCaller with callback method as parameter
            */
            caller.Method(CallBackMethod);
        }

        /*
        * The callback method. This method prints the string sent in the callback
        */
        static void CallBackMethod(string str)
        {
            Console.WriteLine($"Callback was: {str}");
        }    
    }


    public class CallbackCaller
    {
        /*
        * The method that calls back to the caller. Takes an action (method) as parameter
        */
        public void Method(Action<string> callback)
        {
            /*
            * Calls back to method CallBackMet in Program class with the message specified
            */
            callback("The message to send back");
        }
    }
}