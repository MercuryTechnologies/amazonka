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
-- Module      : Amazonka.CognitoIdentityProvider.Types.StringAttributeConstraintsType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.StringAttributeConstraintsType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The constraints associated with a string attribute.
--
-- /See:/ 'newStringAttributeConstraintsType' smart constructor.
data StringAttributeConstraintsType = StringAttributeConstraintsType'
  { -- | The maximum length.
    maxLength :: Prelude.Maybe Prelude.Text,
    -- | The minimum length.
    minLength :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StringAttributeConstraintsType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxLength', 'stringAttributeConstraintsType_maxLength' - The maximum length.
--
-- 'minLength', 'stringAttributeConstraintsType_minLength' - The minimum length.
newStringAttributeConstraintsType ::
  StringAttributeConstraintsType
newStringAttributeConstraintsType =
  StringAttributeConstraintsType'
    { maxLength =
        Prelude.Nothing,
      minLength = Prelude.Nothing
    }

-- | The maximum length.
stringAttributeConstraintsType_maxLength :: Lens.Lens' StringAttributeConstraintsType (Prelude.Maybe Prelude.Text)
stringAttributeConstraintsType_maxLength = Lens.lens (\StringAttributeConstraintsType' {maxLength} -> maxLength) (\s@StringAttributeConstraintsType' {} a -> s {maxLength = a} :: StringAttributeConstraintsType)

-- | The minimum length.
stringAttributeConstraintsType_minLength :: Lens.Lens' StringAttributeConstraintsType (Prelude.Maybe Prelude.Text)
stringAttributeConstraintsType_minLength = Lens.lens (\StringAttributeConstraintsType' {minLength} -> minLength) (\s@StringAttributeConstraintsType' {} a -> s {minLength = a} :: StringAttributeConstraintsType)

instance Core.FromJSON StringAttributeConstraintsType where
  parseJSON =
    Core.withObject
      "StringAttributeConstraintsType"
      ( \x ->
          StringAttributeConstraintsType'
            Prelude.<$> (x Core..:? "MaxLength")
            Prelude.<*> (x Core..:? "MinLength")
      )

instance
  Prelude.Hashable
    StringAttributeConstraintsType
  where
  hashWithSalt
    salt'
    StringAttributeConstraintsType' {..} =
      salt' `Prelude.hashWithSalt` minLength
        `Prelude.hashWithSalt` maxLength

instance
  Prelude.NFData
    StringAttributeConstraintsType
  where
  rnf StringAttributeConstraintsType' {..} =
    Prelude.rnf maxLength
      `Prelude.seq` Prelude.rnf minLength

instance Core.ToJSON StringAttributeConstraintsType where
  toJSON StringAttributeConstraintsType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MaxLength" Core..=) Prelude.<$> maxLength,
            ("MinLength" Core..=) Prelude.<$> minLength
          ]
      )
