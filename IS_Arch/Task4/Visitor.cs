namespace ConsoleApp1
{
    using System;
using System.Collections.Generic;
namespace Visitor.Structural
{
    public static class Program
    {
        public static void Main(string[] args)
        {
            ObjectStructure o = new ObjectStructure();
            o.Attach(new ConcreteElementA());
            o.Attach(new ConcreteElementB());
         
            ConcreteVisitor1 v1 = new ConcreteVisitor1();
            ConcreteVisitor2 v2 = new ConcreteVisitor2();
            
            o.Accept(v1);
            o.Accept(v2);
            
            Console.ReadKey();
        }
    }
    
    
    public abstract class Visitor
    {
        public abstract void VisitConcreteElementA(
            ConcreteElementA concreteElementA);
        public abstract void VisitConcreteElementB(
            ConcreteElementB concreteElementB);
    }
    
    
    public class ConcreteVisitor1 : Visitor
    {
        public override void VisitConcreteElementA(
            ConcreteElementA concreteElementA)
        {
            Console.WriteLine("{0} visited by {1}",
                concreteElementA.GetType().Name, this.GetType().Name);
        }
        public override void VisitConcreteElementB(
            ConcreteElementB concreteElementB)
        {
            Console.WriteLine("{0} visited by {1}",
                concreteElementB.GetType().Name, this.GetType().Name);
        }
    }
    
    
    public class ConcreteVisitor2 : Visitor
    {
        public override void VisitConcreteElementA(
            ConcreteElementA concreteElementA)
        {
            Console.WriteLine("{0} visited by {1}",
                concreteElementA.GetType().Name, this.GetType().Name);
        }
        public override void VisitConcreteElementB(
            ConcreteElementB concreteElementB)
        {
            Console.WriteLine("{0} visited by {1}",
                concreteElementB.GetType().Name, this.GetType().Name);
        }
    }
    
    public abstract class Element
    {
        public abstract void Accept(Visitor visitor);
    }
    
    public class ConcreteElementA : Element
    {
        public override void Accept(Visitor visitor)
        {
            visitor.VisitConcreteElementA(this);
        }
        
        public void OperationA()
        {
        }
    }
    
    public class ConcreteElementB : Element
    {
        public override void Accept(Visitor visitor)
        {
            visitor.VisitConcreteElementB(this);
        }
        public void OperationB()
        {
        }
    }
    
    public class ObjectStructure
    {
        List<Element> elements = new List<Element>();
        public void Attach(Element element)
        {
            elements.Add(element);
        }
        public void Detach(Element element)
        {
            elements.Remove(element);
        }
        public void Accept(Visitor visitor)
        {
            foreach (Element element in elements)
            {
                element.Accept(visitor);
            }
        }
    }
}
}