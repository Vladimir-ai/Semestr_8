using System;

namespace ConsoleApp1
{
    public class Program
    {
        public interface IProduction
        {
            string GetName();
        }

        private class ConcreteProduction : IProduction
        {
            public string GetName()
            {
                return "Concrete";
            }
        }

        private class SteelProduction : IProduction
        {
            public string GetName()
            {
                return "Steel";
            }
        }
        
        private class GravelProduction : IProduction
        {
            public string GetName()
            {
                return "Gravel";
            }
        }

        public enum ProductionType
        {
            Concrete,
            Steel,
            Gravel
        }

        public class Factory
        {
            public IProduction CreateProduction(ProductionType type)
            {
                switch (type)
                {
                    case ProductionType.Concrete:
                        return new ConcreteProduction();
                    case ProductionType.Gravel:
                        return new GravelProduction();
                    case ProductionType.Steel:
                        return new SteelProduction();
                    default:
                        throw new NotSupportedException();
                }
            }
        }

        public static void Main(string[] args)
        {
            Factory factory = new Factory();
            var concrete = factory.CreateProduction(ProductionType.Concrete);
            var gravel = factory.CreateProduction(ProductionType.Gravel);
            var steel = factory.CreateProduction(ProductionType.Steel);
            
            Console.WriteLine(concrete.GetName());
            Console.WriteLine(gravel.GetName());
            Console.WriteLine(steel.GetName());
        }
    }
}