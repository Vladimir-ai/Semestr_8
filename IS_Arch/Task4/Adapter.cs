using System;

namespace ConsoleApp1
{
    public static class Adapter
    {
        public interface ILightningPhone
        {
            void ConnectLightning();
            void Recharge();
        }

        private interface IUsbPhone
        {
            void ConnectUsb();
            void Recharge();
        }

        public class AndroidPhone : IUsbPhone
        {
            private bool _isConnected;
	
            public void ConnectUsb()
            {
                this._isConnected = true;
                Console.WriteLine("Android phone connected.");
            }

            public void Recharge()
            {
                Console.WriteLine(this._isConnected ? "Android phone recharging." : "Connect the USB cable first.");
            }
        }

        private class ApplePhone : ILightningPhone
        {
            private bool _isConnected;
	
            public void ConnectLightning()
            {
                this._isConnected = true;
                Console.WriteLine("Apple phone connected.");
            }

            public void Recharge()
            {
                if (this._isConnected)
                {
                    Console.WriteLine("Apple phone recharging.");
                }
                else
                {
                    Console.WriteLine("Connect the Lightning cable first.");
                }
            }
        }

        private class LightningToUsbAdapter : IUsbPhone
        {
            private readonly ILightningPhone _lightningPhone;
	
            private bool _isConnected;
	
            public LightningToUsbAdapter(ILightningPhone lightningPhone)
            {
                this._lightningPhone = lightningPhone;
                this._lightningPhone.ConnectLightning();
            }
	
            public void ConnectUsb()
            {
                this._isConnected = true;
                Console.WriteLine("Adapter cable connected.");
            }

            public void Recharge()
            {
                if (this._isConnected)
                {
                    this._lightningPhone.Recharge();
                }
                else
                {
                    Console.WriteLine("Connect the USB cable first.");
                }
            }
        }

        public static void Main()
        {
            ILightningPhone applePhone = new ApplePhone();
            IUsbPhone adapterCable = new LightningToUsbAdapter(applePhone);
            adapterCable.ConnectUsb();
            adapterCable.Recharge();
        }
    }
}