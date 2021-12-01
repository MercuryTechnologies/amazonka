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
-- Module      : Amazonka.Pinpoint.Types.CreateTemplateMessageBody
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.CreateTemplateMessageBody where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a request to create a message template.
--
-- /See:/ 'newCreateTemplateMessageBody' smart constructor.
data CreateTemplateMessageBody = CreateTemplateMessageBody'
  { -- | The unique identifier for the request to create the message template.
    requestID :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the message template that was created.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The message that\'s returned from the API for the request to create the
    -- message template.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTemplateMessageBody' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestID', 'createTemplateMessageBody_requestID' - The unique identifier for the request to create the message template.
--
-- 'arn', 'createTemplateMessageBody_arn' - The Amazon Resource Name (ARN) of the message template that was created.
--
-- 'message', 'createTemplateMessageBody_message' - The message that\'s returned from the API for the request to create the
-- message template.
newCreateTemplateMessageBody ::
  CreateTemplateMessageBody
newCreateTemplateMessageBody =
  CreateTemplateMessageBody'
    { requestID =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The unique identifier for the request to create the message template.
createTemplateMessageBody_requestID :: Lens.Lens' CreateTemplateMessageBody (Prelude.Maybe Prelude.Text)
createTemplateMessageBody_requestID = Lens.lens (\CreateTemplateMessageBody' {requestID} -> requestID) (\s@CreateTemplateMessageBody' {} a -> s {requestID = a} :: CreateTemplateMessageBody)

-- | The Amazon Resource Name (ARN) of the message template that was created.
createTemplateMessageBody_arn :: Lens.Lens' CreateTemplateMessageBody (Prelude.Maybe Prelude.Text)
createTemplateMessageBody_arn = Lens.lens (\CreateTemplateMessageBody' {arn} -> arn) (\s@CreateTemplateMessageBody' {} a -> s {arn = a} :: CreateTemplateMessageBody)

-- | The message that\'s returned from the API for the request to create the
-- message template.
createTemplateMessageBody_message :: Lens.Lens' CreateTemplateMessageBody (Prelude.Maybe Prelude.Text)
createTemplateMessageBody_message = Lens.lens (\CreateTemplateMessageBody' {message} -> message) (\s@CreateTemplateMessageBody' {} a -> s {message = a} :: CreateTemplateMessageBody)

instance Core.FromJSON CreateTemplateMessageBody where
  parseJSON =
    Core.withObject
      "CreateTemplateMessageBody"
      ( \x ->
          CreateTemplateMessageBody'
            Prelude.<$> (x Core..:? "RequestID")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Message")
      )

instance Prelude.Hashable CreateTemplateMessageBody where
  hashWithSalt salt' CreateTemplateMessageBody' {..} =
    salt' `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` requestID

instance Prelude.NFData CreateTemplateMessageBody where
  rnf CreateTemplateMessageBody' {..} =
    Prelude.rnf requestID
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf arn
