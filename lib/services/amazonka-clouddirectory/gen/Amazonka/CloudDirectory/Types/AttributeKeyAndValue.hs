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
-- Module      : Amazonka.CloudDirectory.Types.AttributeKeyAndValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.AttributeKeyAndValue where

import Amazonka.CloudDirectory.Types.AttributeKey
import Amazonka.CloudDirectory.Types.TypedAttributeValue
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The combination of an attribute key and an attribute value.
--
-- /See:/ 'newAttributeKeyAndValue' smart constructor.
data AttributeKeyAndValue = AttributeKeyAndValue'
  { -- | The key of the attribute.
    key :: AttributeKey,
    -- | The value of the attribute.
    value :: TypedAttributeValue
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttributeKeyAndValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'attributeKeyAndValue_key' - The key of the attribute.
--
-- 'value', 'attributeKeyAndValue_value' - The value of the attribute.
newAttributeKeyAndValue ::
  -- | 'key'
  AttributeKey ->
  -- | 'value'
  TypedAttributeValue ->
  AttributeKeyAndValue
newAttributeKeyAndValue pKey_ pValue_ =
  AttributeKeyAndValue' {key = pKey_, value = pValue_}

-- | The key of the attribute.
attributeKeyAndValue_key :: Lens.Lens' AttributeKeyAndValue AttributeKey
attributeKeyAndValue_key = Lens.lens (\AttributeKeyAndValue' {key} -> key) (\s@AttributeKeyAndValue' {} a -> s {key = a} :: AttributeKeyAndValue)

-- | The value of the attribute.
attributeKeyAndValue_value :: Lens.Lens' AttributeKeyAndValue TypedAttributeValue
attributeKeyAndValue_value = Lens.lens (\AttributeKeyAndValue' {value} -> value) (\s@AttributeKeyAndValue' {} a -> s {value = a} :: AttributeKeyAndValue)

instance Core.FromJSON AttributeKeyAndValue where
  parseJSON =
    Core.withObject
      "AttributeKeyAndValue"
      ( \x ->
          AttributeKeyAndValue'
            Prelude.<$> (x Core..: "Key") Prelude.<*> (x Core..: "Value")
      )

instance Prelude.Hashable AttributeKeyAndValue where
  hashWithSalt salt' AttributeKeyAndValue' {..} =
    salt' `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` key

instance Prelude.NFData AttributeKeyAndValue where
  rnf AttributeKeyAndValue' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Core.ToJSON AttributeKeyAndValue where
  toJSON AttributeKeyAndValue' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Core..= key),
            Prelude.Just ("Value" Core..= value)
          ]
      )
