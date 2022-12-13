#pragma once

class Function;

class FunctionPass
{
  public:
    virtual bool RunOnFunction(Function &F) = 0;
};
