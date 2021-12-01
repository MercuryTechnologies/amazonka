{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Greengrass.Types.FunctionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Greengrass.Types.FunctionConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.Greengrass.Types.EncodingType
import Amazonka.Greengrass.Types.FunctionConfigurationEnvironment
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration of the Lambda function.
--
-- /See:/ 'newFunctionConfiguration' smart constructor.
data FunctionConfiguration = FunctionConfiguration'
  { -- | The memory size, in KB, which the function requires. This setting is not
    -- applicable and should be cleared when you run the Lambda function
    -- without containerization.
    memorySize :: Prelude.Maybe Prelude.Int,
    -- | The execution arguments.
    execArgs :: Prelude.Maybe Prelude.Text,
    -- | The environment configuration of the function.
    environment :: Prelude.Maybe FunctionConfigurationEnvironment,
    -- | The name of the function executable.
    executable :: Prelude.Maybe Prelude.Text,
    -- | True if the function is pinned. Pinned means the function is long-lived
    -- and starts when the core starts.
    pinned :: Prelude.Maybe Prelude.Bool,
    -- | The expected encoding type of the input payload for the function. The
    -- default is \'\'json\'\'.
    encodingType :: Prelude.Maybe EncodingType,
    -- | The allowed function execution time, after which Lambda should terminate
    -- the function. This timeout still applies to pinned Lambda functions for
    -- each request.
    timeout :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunctionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memorySize', 'functionConfiguration_memorySize' - The memory size, in KB, which the function requires. This setting is not
-- applicable and should be cleared when you run the Lambda function
-- without containerization.
--
-- 'execArgs', 'functionConfiguration_execArgs' - The execution arguments.
--
-- 'environment', 'functionConfiguration_environment' - The environment configuration of the function.
--
-- 'executable', 'functionConfiguration_executable' - The name of the function executable.
--
-- 'pinned', 'functionConfiguration_pinned' - True if the function is pinned. Pinned means the function is long-lived
-- and starts when the core starts.
--
-- 'encodingType', 'functionConfiguration_encodingType' - The expected encoding type of the input payload for the function. The
-- default is \'\'json\'\'.
--
-- 'timeout', 'functionConfiguration_timeout' - The allowed function execution time, after which Lambda should terminate
-- the function. This timeout still applies to pinned Lambda functions for
-- each request.
newFunctionConfiguration ::
  FunctionConfiguration
newFunctionConfiguration =
  FunctionConfiguration'
    { memorySize =
        Prelude.Nothing,
      execArgs = Prelude.Nothing,
      environment = Prelude.Nothing,
      executable = Prelude.Nothing,
      pinned = Prelude.Nothing,
      encodingType = Prelude.Nothing,
      timeout = Prelude.Nothing
    }

-- | The memory size, in KB, which the function requires. This setting is not
-- applicable and should be cleared when you run the Lambda function
-- without containerization.
functionConfiguration_memorySize :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Int)
functionConfiguration_memorySize = Lens.lens (\FunctionConfiguration' {memorySize} -> memorySize) (\s@FunctionConfiguration' {} a -> s {memorySize = a} :: FunctionConfiguration)

-- | The execution arguments.
functionConfiguration_execArgs :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_execArgs = Lens.lens (\FunctionConfiguration' {execArgs} -> execArgs) (\s@FunctionConfiguration' {} a -> s {execArgs = a} :: FunctionConfiguration)

-- | The environment configuration of the function.
functionConfiguration_environment :: Lens.Lens' FunctionConfiguration (Prelude.Maybe FunctionConfigurationEnvironment)
functionConfiguration_environment = Lens.lens (\FunctionConfiguration' {environment} -> environment) (\s@FunctionConfiguration' {} a -> s {environment = a} :: FunctionConfiguration)

-- | The name of the function executable.
functionConfiguration_executable :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Text)
functionConfiguration_executable = Lens.lens (\FunctionConfiguration' {executable} -> executable) (\s@FunctionConfiguration' {} a -> s {executable = a} :: FunctionConfiguration)

-- | True if the function is pinned. Pinned means the function is long-lived
-- and starts when the core starts.
functionConfiguration_pinned :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Bool)
functionConfiguration_pinned = Lens.lens (\FunctionConfiguration' {pinned} -> pinned) (\s@FunctionConfiguration' {} a -> s {pinned = a} :: FunctionConfiguration)

-- | The expected encoding type of the input payload for the function. The
-- default is \'\'json\'\'.
functionConfiguration_encodingType :: Lens.Lens' FunctionConfiguration (Prelude.Maybe EncodingType)
functionConfiguration_encodingType = Lens.lens (\FunctionConfiguration' {encodingType} -> encodingType) (\s@FunctionConfiguration' {} a -> s {encodingType = a} :: FunctionConfiguration)

-- | The allowed function execution time, after which Lambda should terminate
-- the function. This timeout still applies to pinned Lambda functions for
-- each request.
functionConfiguration_timeout :: Lens.Lens' FunctionConfiguration (Prelude.Maybe Prelude.Int)
functionConfiguration_timeout = Lens.lens (\FunctionConfiguration' {timeout} -> timeout) (\s@FunctionConfiguration' {} a -> s {timeout = a} :: FunctionConfiguration)

instance Core.FromJSON FunctionConfiguration where
  parseJSON =
    Core.withObject
      "FunctionConfiguration"
      ( \x ->
          FunctionConfiguration'
            Prelude.<$> (x Core..:? "MemorySize")
            Prelude.<*> (x Core..:? "ExecArgs")
            Prelude.<*> (x Core..:? "Environment")
            Prelude.<*> (x Core..:? "Executable")
            Prelude.<*> (x Core..:? "Pinned")
            Prelude.<*> (x Core..:? "EncodingType")
            Prelude.<*> (x Core..:? "Timeout")
      )

instance Prelude.Hashable FunctionConfiguration where
  hashWithSalt salt' FunctionConfiguration' {..} =
    salt' `Prelude.hashWithSalt` timeout
      `Prelude.hashWithSalt` encodingType
      `Prelude.hashWithSalt` pinned
      `Prelude.hashWithSalt` executable
      `Prelude.hashWithSalt` environment
      `Prelude.hashWithSalt` execArgs
      `Prelude.hashWithSalt` memorySize

instance Prelude.NFData FunctionConfiguration where
  rnf FunctionConfiguration' {..} =
    Prelude.rnf memorySize
      `Prelude.seq` Prelude.rnf timeout
      `Prelude.seq` Prelude.rnf encodingType
      `Prelude.seq` Prelude.rnf pinned
      `Prelude.seq` Prelude.rnf executable
      `Prelude.seq` Prelude.rnf environment
      `Prelude.seq` Prelude.rnf execArgs

instance Core.ToJSON FunctionConfiguration where
  toJSON FunctionConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MemorySize" Core..=) Prelude.<$> memorySize,
            ("ExecArgs" Core..=) Prelude.<$> execArgs,
            ("Environment" Core..=) Prelude.<$> environment,
            ("Executable" Core..=) Prelude.<$> executable,
            ("Pinned" Core..=) Prelude.<$> pinned,
            ("EncodingType" Core..=) Prelude.<$> encodingType,
            ("Timeout" Core..=) Prelude.<$> timeout
          ]
      )
