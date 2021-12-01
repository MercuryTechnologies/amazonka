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
-- Module      : Amazonka.StepFunctions.Types.LambdaFunctionStartFailedEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.LambdaFunctionStartFailedEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains details about a lambda function that failed to start during an
-- execution.
--
-- /See:/ 'newLambdaFunctionStartFailedEventDetails' smart constructor.
data LambdaFunctionStartFailedEventDetails = LambdaFunctionStartFailedEventDetails'
  { -- | The error code of the failure.
    error :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A more detailed explanation of the cause of the failure.
    cause :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionStartFailedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'lambdaFunctionStartFailedEventDetails_error' - The error code of the failure.
--
-- 'cause', 'lambdaFunctionStartFailedEventDetails_cause' - A more detailed explanation of the cause of the failure.
newLambdaFunctionStartFailedEventDetails ::
  LambdaFunctionStartFailedEventDetails
newLambdaFunctionStartFailedEventDetails =
  LambdaFunctionStartFailedEventDetails'
    { error =
        Prelude.Nothing,
      cause = Prelude.Nothing
    }

-- | The error code of the failure.
lambdaFunctionStartFailedEventDetails_error :: Lens.Lens' LambdaFunctionStartFailedEventDetails (Prelude.Maybe Prelude.Text)
lambdaFunctionStartFailedEventDetails_error = Lens.lens (\LambdaFunctionStartFailedEventDetails' {error} -> error) (\s@LambdaFunctionStartFailedEventDetails' {} a -> s {error = a} :: LambdaFunctionStartFailedEventDetails) Prelude.. Lens.mapping Core._Sensitive

-- | A more detailed explanation of the cause of the failure.
lambdaFunctionStartFailedEventDetails_cause :: Lens.Lens' LambdaFunctionStartFailedEventDetails (Prelude.Maybe Prelude.Text)
lambdaFunctionStartFailedEventDetails_cause = Lens.lens (\LambdaFunctionStartFailedEventDetails' {cause} -> cause) (\s@LambdaFunctionStartFailedEventDetails' {} a -> s {cause = a} :: LambdaFunctionStartFailedEventDetails) Prelude.. Lens.mapping Core._Sensitive

instance
  Core.FromJSON
    LambdaFunctionStartFailedEventDetails
  where
  parseJSON =
    Core.withObject
      "LambdaFunctionStartFailedEventDetails"
      ( \x ->
          LambdaFunctionStartFailedEventDetails'
            Prelude.<$> (x Core..:? "error")
            Prelude.<*> (x Core..:? "cause")
      )

instance
  Prelude.Hashable
    LambdaFunctionStartFailedEventDetails
  where
  hashWithSalt
    salt'
    LambdaFunctionStartFailedEventDetails' {..} =
      salt' `Prelude.hashWithSalt` cause
        `Prelude.hashWithSalt` error

instance
  Prelude.NFData
    LambdaFunctionStartFailedEventDetails
  where
  rnf LambdaFunctionStartFailedEventDetails' {..} =
    Prelude.rnf error `Prelude.seq` Prelude.rnf cause
