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
-- Module      : Amazonka.DataBrew.Types.RecipeVersionErrorDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.RecipeVersionErrorDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents any errors encountered when attempting to delete multiple
-- recipe versions.
--
-- /See:/ 'newRecipeVersionErrorDetail' smart constructor.
data RecipeVersionErrorDetail = RecipeVersionErrorDetail'
  { -- | The identifier for the recipe version associated with this error.
    recipeVersion :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status code for the error.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The text of the error message.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecipeVersionErrorDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recipeVersion', 'recipeVersionErrorDetail_recipeVersion' - The identifier for the recipe version associated with this error.
--
-- 'errorCode', 'recipeVersionErrorDetail_errorCode' - The HTTP status code for the error.
--
-- 'errorMessage', 'recipeVersionErrorDetail_errorMessage' - The text of the error message.
newRecipeVersionErrorDetail ::
  RecipeVersionErrorDetail
newRecipeVersionErrorDetail =
  RecipeVersionErrorDetail'
    { recipeVersion =
        Prelude.Nothing,
      errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | The identifier for the recipe version associated with this error.
recipeVersionErrorDetail_recipeVersion :: Lens.Lens' RecipeVersionErrorDetail (Prelude.Maybe Prelude.Text)
recipeVersionErrorDetail_recipeVersion = Lens.lens (\RecipeVersionErrorDetail' {recipeVersion} -> recipeVersion) (\s@RecipeVersionErrorDetail' {} a -> s {recipeVersion = a} :: RecipeVersionErrorDetail)

-- | The HTTP status code for the error.
recipeVersionErrorDetail_errorCode :: Lens.Lens' RecipeVersionErrorDetail (Prelude.Maybe Prelude.Text)
recipeVersionErrorDetail_errorCode = Lens.lens (\RecipeVersionErrorDetail' {errorCode} -> errorCode) (\s@RecipeVersionErrorDetail' {} a -> s {errorCode = a} :: RecipeVersionErrorDetail)

-- | The text of the error message.
recipeVersionErrorDetail_errorMessage :: Lens.Lens' RecipeVersionErrorDetail (Prelude.Maybe Prelude.Text)
recipeVersionErrorDetail_errorMessage = Lens.lens (\RecipeVersionErrorDetail' {errorMessage} -> errorMessage) (\s@RecipeVersionErrorDetail' {} a -> s {errorMessage = a} :: RecipeVersionErrorDetail)

instance Core.FromJSON RecipeVersionErrorDetail where
  parseJSON =
    Core.withObject
      "RecipeVersionErrorDetail"
      ( \x ->
          RecipeVersionErrorDetail'
            Prelude.<$> (x Core..:? "RecipeVersion")
            Prelude.<*> (x Core..:? "ErrorCode")
            Prelude.<*> (x Core..:? "ErrorMessage")
      )

instance Prelude.Hashable RecipeVersionErrorDetail where
  hashWithSalt salt' RecipeVersionErrorDetail' {..} =
    salt' `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` recipeVersion

instance Prelude.NFData RecipeVersionErrorDetail where
  rnf RecipeVersionErrorDetail' {..} =
    Prelude.rnf recipeVersion
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf errorCode
