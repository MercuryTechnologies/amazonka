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
-- Module      : Amazonka.SSMContacts.Types.Contact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types.Contact where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMContacts.Types.ContactType

-- | A personal contact or escalation plan that Incident Manager engages
-- during an incident.
--
-- /See:/ 'newContact' smart constructor.
data Contact = Contact'
  { -- | The full name of the contact or escalation plan.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the contact or escalation plan.
    contactArn :: Prelude.Text,
    -- | The unique and identifiable alias of the contact or escalation plan.
    alias :: Prelude.Text,
    -- | Refers to the type of contact. A single contact is type @PERSONAL@ and
    -- an escalation plan is type @ESCALATION@.
    type' :: ContactType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Contact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'displayName', 'contact_displayName' - The full name of the contact or escalation plan.
--
-- 'contactArn', 'contact_contactArn' - The Amazon Resource Name (ARN) of the contact or escalation plan.
--
-- 'alias', 'contact_alias' - The unique and identifiable alias of the contact or escalation plan.
--
-- 'type'', 'contact_type' - Refers to the type of contact. A single contact is type @PERSONAL@ and
-- an escalation plan is type @ESCALATION@.
newContact ::
  -- | 'contactArn'
  Prelude.Text ->
  -- | 'alias'
  Prelude.Text ->
  -- | 'type''
  ContactType ->
  Contact
newContact pContactArn_ pAlias_ pType_ =
  Contact'
    { displayName = Prelude.Nothing,
      contactArn = pContactArn_,
      alias = pAlias_,
      type' = pType_
    }

-- | The full name of the contact or escalation plan.
contact_displayName :: Lens.Lens' Contact (Prelude.Maybe Prelude.Text)
contact_displayName = Lens.lens (\Contact' {displayName} -> displayName) (\s@Contact' {} a -> s {displayName = a} :: Contact)

-- | The Amazon Resource Name (ARN) of the contact or escalation plan.
contact_contactArn :: Lens.Lens' Contact Prelude.Text
contact_contactArn = Lens.lens (\Contact' {contactArn} -> contactArn) (\s@Contact' {} a -> s {contactArn = a} :: Contact)

-- | The unique and identifiable alias of the contact or escalation plan.
contact_alias :: Lens.Lens' Contact Prelude.Text
contact_alias = Lens.lens (\Contact' {alias} -> alias) (\s@Contact' {} a -> s {alias = a} :: Contact)

-- | Refers to the type of contact. A single contact is type @PERSONAL@ and
-- an escalation plan is type @ESCALATION@.
contact_type :: Lens.Lens' Contact ContactType
contact_type = Lens.lens (\Contact' {type'} -> type') (\s@Contact' {} a -> s {type' = a} :: Contact)

instance Core.FromJSON Contact where
  parseJSON =
    Core.withObject
      "Contact"
      ( \x ->
          Contact'
            Prelude.<$> (x Core..:? "DisplayName")
            Prelude.<*> (x Core..: "ContactArn")
            Prelude.<*> (x Core..: "Alias")
            Prelude.<*> (x Core..: "Type")
      )

instance Prelude.Hashable Contact where
  hashWithSalt salt' Contact' {..} =
    salt' `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` alias
      `Prelude.hashWithSalt` contactArn
      `Prelude.hashWithSalt` displayName

instance Prelude.NFData Contact where
  rnf Contact' {..} =
    Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf alias
      `Prelude.seq` Prelude.rnf contactArn
