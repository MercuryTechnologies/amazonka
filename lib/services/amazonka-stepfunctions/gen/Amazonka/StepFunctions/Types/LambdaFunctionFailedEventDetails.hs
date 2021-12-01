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
-- Module      : Amazonka.StepFunctions.Types.LambdaFunctionFailedEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.LambdaFunctionFailedEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains details about a lambda function that failed during an
-- execution.
--
-- /See:/ 'newLambdaFunctionFailedEventDetails' smart constructor.
data LambdaFunctionFailedEventDetails = LambdaFunctionFailedEventDetails'
  { -- | The error code of the failure.
    error :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A more detailed explanation of the cause of the failure.
    cause :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaFunctionFailedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'lambdaFunctionFailedEventDetails_error' - The error code of the failure.
--
-- 'cause', 'lambdaFunctionFailedEventDetails_cause' - A more detailed explanation of the cause of the failure.
newLambdaFunctionFailedEventDetails ::
  LambdaFunctionFailedEventDetails
newLambdaFunctionFailedEventDetails =
  LambdaFunctionFailedEventDetails'
    { error =
        Prelude.Nothing,
      cause = Prelude.Nothing
    }

-- | The error code of the failure.
lambdaFunctionFailedEventDetails_error :: Lens.Lens' LambdaFunctionFailedEventDetails (Prelude.Maybe Prelude.Text)
lambdaFunctionFailedEventDetails_error = Lens.lens (\LambdaFunctionFailedEventDetails' {error} -> error) (\s@LambdaFunctionFailedEventDetails' {} a -> s {error = a} :: LambdaFunctionFailedEventDetails) Prelude.. Lens.mapping Core._Sensitive

-- | A more detailed explanation of the cause of the failure.
lambdaFunctionFailedEventDetails_cause :: Lens.Lens' LambdaFunctionFailedEventDetails (Prelude.Maybe Prelude.Text)
lambdaFunctionFailedEventDetails_cause = Lens.lens (\LambdaFunctionFailedEventDetails' {cause} -> cause) (\s@LambdaFunctionFailedEventDetails' {} a -> s {cause = a} :: LambdaFunctionFailedEventDetails) Prelude.. Lens.mapping Core._Sensitive

instance
  Core.FromJSON
    LambdaFunctionFailedEventDetails
  where
  parseJSON =
    Core.withObject
      "LambdaFunctionFailedEventDetails"
      ( \x ->
          LambdaFunctionFailedEventDetails'
            Prelude.<$> (x Core..:? "error")
            Prelude.<*> (x Core..:? "cause")
      )

instance
  Prelude.Hashable
    LambdaFunctionFailedEventDetails
  where
  hashWithSalt
    salt'
    LambdaFunctionFailedEventDetails' {..} =
      salt' `Prelude.hashWithSalt` cause
        `Prelude.hashWithSalt` error

instance
  Prelude.NFData
    LambdaFunctionFailedEventDetails
  where
  rnf LambdaFunctionFailedEventDetails' {..} =
    Prelude.rnf error `Prelude.seq` Prelude.rnf cause
