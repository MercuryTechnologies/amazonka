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
-- Module      : Amazonka.CloudDirectory.Types.TypedAttributeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.TypedAttributeValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the data for a typed attribute. You can set one, and only
-- one, of the elements. Each attribute in an item is a name-value pair.
-- Attributes have a single value.
--
-- /See:/ 'newTypedAttributeValue' smart constructor.
data TypedAttributeValue = TypedAttributeValue'
  { -- | A binary data value.
    binaryValue :: Prelude.Maybe Core.Base64,
    -- | A date and time value.
    datetimeValue :: Prelude.Maybe Core.POSIX,
    -- | A number data value.
    numberValue :: Prelude.Maybe Prelude.Text,
    -- | A string data value.
    stringValue :: Prelude.Maybe Prelude.Text,
    -- | A Boolean data value.
    booleanValue :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TypedAttributeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'binaryValue', 'typedAttributeValue_binaryValue' - A binary data value.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'datetimeValue', 'typedAttributeValue_datetimeValue' - A date and time value.
--
-- 'numberValue', 'typedAttributeValue_numberValue' - A number data value.
--
-- 'stringValue', 'typedAttributeValue_stringValue' - A string data value.
--
-- 'booleanValue', 'typedAttributeValue_booleanValue' - A Boolean data value.
newTypedAttributeValue ::
  TypedAttributeValue
newTypedAttributeValue =
  TypedAttributeValue'
    { binaryValue = Prelude.Nothing,
      datetimeValue = Prelude.Nothing,
      numberValue = Prelude.Nothing,
      stringValue = Prelude.Nothing,
      booleanValue = Prelude.Nothing
    }

-- | A binary data value.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
typedAttributeValue_binaryValue :: Lens.Lens' TypedAttributeValue (Prelude.Maybe Prelude.ByteString)
typedAttributeValue_binaryValue = Lens.lens (\TypedAttributeValue' {binaryValue} -> binaryValue) (\s@TypedAttributeValue' {} a -> s {binaryValue = a} :: TypedAttributeValue) Prelude.. Lens.mapping Core._Base64

-- | A date and time value.
typedAttributeValue_datetimeValue :: Lens.Lens' TypedAttributeValue (Prelude.Maybe Prelude.UTCTime)
typedAttributeValue_datetimeValue = Lens.lens (\TypedAttributeValue' {datetimeValue} -> datetimeValue) (\s@TypedAttributeValue' {} a -> s {datetimeValue = a} :: TypedAttributeValue) Prelude.. Lens.mapping Core._Time

-- | A number data value.
typedAttributeValue_numberValue :: Lens.Lens' TypedAttributeValue (Prelude.Maybe Prelude.Text)
typedAttributeValue_numberValue = Lens.lens (\TypedAttributeValue' {numberValue} -> numberValue) (\s@TypedAttributeValue' {} a -> s {numberValue = a} :: TypedAttributeValue)

-- | A string data value.
typedAttributeValue_stringValue :: Lens.Lens' TypedAttributeValue (Prelude.Maybe Prelude.Text)
typedAttributeValue_stringValue = Lens.lens (\TypedAttributeValue' {stringValue} -> stringValue) (\s@TypedAttributeValue' {} a -> s {stringValue = a} :: TypedAttributeValue)

-- | A Boolean data value.
typedAttributeValue_booleanValue :: Lens.Lens' TypedAttributeValue (Prelude.Maybe Prelude.Bool)
typedAttributeValue_booleanValue = Lens.lens (\TypedAttributeValue' {booleanValue} -> booleanValue) (\s@TypedAttributeValue' {} a -> s {booleanValue = a} :: TypedAttributeValue)

instance Core.FromJSON TypedAttributeValue where
  parseJSON =
    Core.withObject
      "TypedAttributeValue"
      ( \x ->
          TypedAttributeValue'
            Prelude.<$> (x Core..:? "BinaryValue")
            Prelude.<*> (x Core..:? "DatetimeValue")
            Prelude.<*> (x Core..:? "NumberValue")
            Prelude.<*> (x Core..:? "StringValue")
            Prelude.<*> (x Core..:? "BooleanValue")
      )

instance Prelude.Hashable TypedAttributeValue where
  hashWithSalt salt' TypedAttributeValue' {..} =
    salt' `Prelude.hashWithSalt` booleanValue
      `Prelude.hashWithSalt` stringValue
      `Prelude.hashWithSalt` numberValue
      `Prelude.hashWithSalt` datetimeValue
      `Prelude.hashWithSalt` binaryValue

instance Prelude.NFData TypedAttributeValue where
  rnf TypedAttributeValue' {..} =
    Prelude.rnf binaryValue
      `Prelude.seq` Prelude.rnf booleanValue
      `Prelude.seq` Prelude.rnf stringValue
      `Prelude.seq` Prelude.rnf numberValue
      `Prelude.seq` Prelude.rnf datetimeValue

instance Core.ToJSON TypedAttributeValue where
  toJSON TypedAttributeValue' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("BinaryValue" Core..=) Prelude.<$> binaryValue,
            ("DatetimeValue" Core..=) Prelude.<$> datetimeValue,
            ("NumberValue" Core..=) Prelude.<$> numberValue,
            ("StringValue" Core..=) Prelude.<$> stringValue,
            ("BooleanValue" Core..=) Prelude.<$> booleanValue
          ]
      )
