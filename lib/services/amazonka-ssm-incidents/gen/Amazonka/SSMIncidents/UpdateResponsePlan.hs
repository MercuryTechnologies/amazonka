{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SSMIncidents.UpdateResponsePlan
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified response plan.
module Amazonka.SSMIncidents.UpdateResponsePlan
  ( -- * Creating a Request
    UpdateResponsePlan (..),
    newUpdateResponsePlan,

    -- * Request Lenses
    updateResponsePlan_incidentTemplateImpact,
    updateResponsePlan_clientToken,
    updateResponsePlan_actions,
    updateResponsePlan_incidentTemplateSummary,
    updateResponsePlan_displayName,
    updateResponsePlan_chatChannel,
    updateResponsePlan_incidentTemplateDedupeString,
    updateResponsePlan_incidentTemplateTitle,
    updateResponsePlan_engagements,
    updateResponsePlan_incidentTemplateNotificationTargets,
    updateResponsePlan_arn,

    -- * Destructuring the Response
    UpdateResponsePlanResponse (..),
    newUpdateResponsePlanResponse,

    -- * Response Lenses
    updateResponsePlanResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newUpdateResponsePlan' smart constructor.
data UpdateResponsePlan = UpdateResponsePlan'
  { -- | Defines the impact to the customers. Providing an impact overwrites the
    -- impact provided by a response plan.
    --
    -- __Possible impacts:__
    --
    -- -   @5@ - Severe impact
    --
    -- -   @4@ - High impact
    --
    -- -   @3@ - Medium impact
    --
    -- -   @2@ - Low impact
    --
    -- -   @1@ - No impact
    incidentTemplateImpact :: Prelude.Maybe Prelude.Natural,
    -- | A token ensuring that the action is called only once with the specified
    -- details.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The actions that this response plan takes at the beginning of an
    -- incident.
    actions :: Prelude.Maybe [Action],
    -- | A brief summary of the incident. This typically contains what has
    -- happened, what\'s currently happening, and next steps.
    incidentTemplateSummary :: Prelude.Maybe Prelude.Text,
    -- | The long format name of the response plan. Can\'t contain spaces.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The AWS Chatbot chat channel used for collaboration during an incident.
    --
    -- Use the empty structure to remove the chat channel from the response
    -- plan.
    chatChannel :: Prelude.Maybe ChatChannel,
    -- | Used to create only one incident record for an incident.
    incidentTemplateDedupeString :: Prelude.Maybe Prelude.Text,
    -- | The short format name of the incident. Can\'t contain spaces.
    incidentTemplateTitle :: Prelude.Maybe Prelude.Text,
    -- | The contacts and escalation plans that Incident Manager engages at the
    -- start of the incident.
    engagements :: Prelude.Maybe [Prelude.Text],
    -- | The SNS targets that are notified when updates are made to an incident.
    incidentTemplateNotificationTargets :: Prelude.Maybe [NotificationTargetItem],
    -- | The Amazon Resource Name (ARN) of the response plan.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResponsePlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'incidentTemplateImpact', 'updateResponsePlan_incidentTemplateImpact' - Defines the impact to the customers. Providing an impact overwrites the
-- impact provided by a response plan.
--
-- __Possible impacts:__
--
-- -   @5@ - Severe impact
--
-- -   @4@ - High impact
--
-- -   @3@ - Medium impact
--
-- -   @2@ - Low impact
--
-- -   @1@ - No impact
--
-- 'clientToken', 'updateResponsePlan_clientToken' - A token ensuring that the action is called only once with the specified
-- details.
--
-- 'actions', 'updateResponsePlan_actions' - The actions that this response plan takes at the beginning of an
-- incident.
--
-- 'incidentTemplateSummary', 'updateResponsePlan_incidentTemplateSummary' - A brief summary of the incident. This typically contains what has
-- happened, what\'s currently happening, and next steps.
--
-- 'displayName', 'updateResponsePlan_displayName' - The long format name of the response plan. Can\'t contain spaces.
--
-- 'chatChannel', 'updateResponsePlan_chatChannel' - The AWS Chatbot chat channel used for collaboration during an incident.
--
-- Use the empty structure to remove the chat channel from the response
-- plan.
--
-- 'incidentTemplateDedupeString', 'updateResponsePlan_incidentTemplateDedupeString' - Used to create only one incident record for an incident.
--
-- 'incidentTemplateTitle', 'updateResponsePlan_incidentTemplateTitle' - The short format name of the incident. Can\'t contain spaces.
--
-- 'engagements', 'updateResponsePlan_engagements' - The contacts and escalation plans that Incident Manager engages at the
-- start of the incident.
--
-- 'incidentTemplateNotificationTargets', 'updateResponsePlan_incidentTemplateNotificationTargets' - The SNS targets that are notified when updates are made to an incident.
--
-- 'arn', 'updateResponsePlan_arn' - The Amazon Resource Name (ARN) of the response plan.
newUpdateResponsePlan ::
  -- | 'arn'
  Prelude.Text ->
  UpdateResponsePlan
newUpdateResponsePlan pArn_ =
  UpdateResponsePlan'
    { incidentTemplateImpact =
        Prelude.Nothing,
      clientToken = Prelude.Nothing,
      actions = Prelude.Nothing,
      incidentTemplateSummary = Prelude.Nothing,
      displayName = Prelude.Nothing,
      chatChannel = Prelude.Nothing,
      incidentTemplateDedupeString = Prelude.Nothing,
      incidentTemplateTitle = Prelude.Nothing,
      engagements = Prelude.Nothing,
      incidentTemplateNotificationTargets =
        Prelude.Nothing,
      arn = pArn_
    }

-- | Defines the impact to the customers. Providing an impact overwrites the
-- impact provided by a response plan.
--
-- __Possible impacts:__
--
-- -   @5@ - Severe impact
--
-- -   @4@ - High impact
--
-- -   @3@ - Medium impact
--
-- -   @2@ - Low impact
--
-- -   @1@ - No impact
updateResponsePlan_incidentTemplateImpact :: Lens.Lens' UpdateResponsePlan (Prelude.Maybe Prelude.Natural)
updateResponsePlan_incidentTemplateImpact = Lens.lens (\UpdateResponsePlan' {incidentTemplateImpact} -> incidentTemplateImpact) (\s@UpdateResponsePlan' {} a -> s {incidentTemplateImpact = a} :: UpdateResponsePlan)

-- | A token ensuring that the action is called only once with the specified
-- details.
updateResponsePlan_clientToken :: Lens.Lens' UpdateResponsePlan (Prelude.Maybe Prelude.Text)
updateResponsePlan_clientToken = Lens.lens (\UpdateResponsePlan' {clientToken} -> clientToken) (\s@UpdateResponsePlan' {} a -> s {clientToken = a} :: UpdateResponsePlan)

-- | The actions that this response plan takes at the beginning of an
-- incident.
updateResponsePlan_actions :: Lens.Lens' UpdateResponsePlan (Prelude.Maybe [Action])
updateResponsePlan_actions = Lens.lens (\UpdateResponsePlan' {actions} -> actions) (\s@UpdateResponsePlan' {} a -> s {actions = a} :: UpdateResponsePlan) Prelude.. Lens.mapping Lens.coerced

-- | A brief summary of the incident. This typically contains what has
-- happened, what\'s currently happening, and next steps.
updateResponsePlan_incidentTemplateSummary :: Lens.Lens' UpdateResponsePlan (Prelude.Maybe Prelude.Text)
updateResponsePlan_incidentTemplateSummary = Lens.lens (\UpdateResponsePlan' {incidentTemplateSummary} -> incidentTemplateSummary) (\s@UpdateResponsePlan' {} a -> s {incidentTemplateSummary = a} :: UpdateResponsePlan)

-- | The long format name of the response plan. Can\'t contain spaces.
updateResponsePlan_displayName :: Lens.Lens' UpdateResponsePlan (Prelude.Maybe Prelude.Text)
updateResponsePlan_displayName = Lens.lens (\UpdateResponsePlan' {displayName} -> displayName) (\s@UpdateResponsePlan' {} a -> s {displayName = a} :: UpdateResponsePlan)

-- | The AWS Chatbot chat channel used for collaboration during an incident.
--
-- Use the empty structure to remove the chat channel from the response
-- plan.
updateResponsePlan_chatChannel :: Lens.Lens' UpdateResponsePlan (Prelude.Maybe ChatChannel)
updateResponsePlan_chatChannel = Lens.lens (\UpdateResponsePlan' {chatChannel} -> chatChannel) (\s@UpdateResponsePlan' {} a -> s {chatChannel = a} :: UpdateResponsePlan)

-- | Used to create only one incident record for an incident.
updateResponsePlan_incidentTemplateDedupeString :: Lens.Lens' UpdateResponsePlan (Prelude.Maybe Prelude.Text)
updateResponsePlan_incidentTemplateDedupeString = Lens.lens (\UpdateResponsePlan' {incidentTemplateDedupeString} -> incidentTemplateDedupeString) (\s@UpdateResponsePlan' {} a -> s {incidentTemplateDedupeString = a} :: UpdateResponsePlan)

-- | The short format name of the incident. Can\'t contain spaces.
updateResponsePlan_incidentTemplateTitle :: Lens.Lens' UpdateResponsePlan (Prelude.Maybe Prelude.Text)
updateResponsePlan_incidentTemplateTitle = Lens.lens (\UpdateResponsePlan' {incidentTemplateTitle} -> incidentTemplateTitle) (\s@UpdateResponsePlan' {} a -> s {incidentTemplateTitle = a} :: UpdateResponsePlan)

-- | The contacts and escalation plans that Incident Manager engages at the
-- start of the incident.
updateResponsePlan_engagements :: Lens.Lens' UpdateResponsePlan (Prelude.Maybe [Prelude.Text])
updateResponsePlan_engagements = Lens.lens (\UpdateResponsePlan' {engagements} -> engagements) (\s@UpdateResponsePlan' {} a -> s {engagements = a} :: UpdateResponsePlan) Prelude.. Lens.mapping Lens.coerced

-- | The SNS targets that are notified when updates are made to an incident.
updateResponsePlan_incidentTemplateNotificationTargets :: Lens.Lens' UpdateResponsePlan (Prelude.Maybe [NotificationTargetItem])
updateResponsePlan_incidentTemplateNotificationTargets = Lens.lens (\UpdateResponsePlan' {incidentTemplateNotificationTargets} -> incidentTemplateNotificationTargets) (\s@UpdateResponsePlan' {} a -> s {incidentTemplateNotificationTargets = a} :: UpdateResponsePlan) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the response plan.
updateResponsePlan_arn :: Lens.Lens' UpdateResponsePlan Prelude.Text
updateResponsePlan_arn = Lens.lens (\UpdateResponsePlan' {arn} -> arn) (\s@UpdateResponsePlan' {} a -> s {arn = a} :: UpdateResponsePlan)

instance Core.AWSRequest UpdateResponsePlan where
  type
    AWSResponse UpdateResponsePlan =
      UpdateResponsePlanResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateResponsePlanResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateResponsePlan where
  hashWithSalt salt' UpdateResponsePlan' {..} =
    salt' `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` incidentTemplateNotificationTargets
      `Prelude.hashWithSalt` engagements
      `Prelude.hashWithSalt` incidentTemplateTitle
      `Prelude.hashWithSalt` incidentTemplateDedupeString
      `Prelude.hashWithSalt` chatChannel
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` incidentTemplateSummary
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` incidentTemplateImpact

instance Prelude.NFData UpdateResponsePlan where
  rnf UpdateResponsePlan' {..} =
    Prelude.rnf incidentTemplateImpact
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf incidentTemplateNotificationTargets
      `Prelude.seq` Prelude.rnf engagements
      `Prelude.seq` Prelude.rnf incidentTemplateTitle
      `Prelude.seq` Prelude.rnf incidentTemplateDedupeString
      `Prelude.seq` Prelude.rnf chatChannel
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf incidentTemplateSummary
      `Prelude.seq` Prelude.rnf actions
      `Prelude.seq` Prelude.rnf clientToken

instance Core.ToHeaders UpdateResponsePlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateResponsePlan where
  toJSON UpdateResponsePlan' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("incidentTemplateImpact" Core..=)
              Prelude.<$> incidentTemplateImpact,
            ("clientToken" Core..=) Prelude.<$> clientToken,
            ("actions" Core..=) Prelude.<$> actions,
            ("incidentTemplateSummary" Core..=)
              Prelude.<$> incidentTemplateSummary,
            ("displayName" Core..=) Prelude.<$> displayName,
            ("chatChannel" Core..=) Prelude.<$> chatChannel,
            ("incidentTemplateDedupeString" Core..=)
              Prelude.<$> incidentTemplateDedupeString,
            ("incidentTemplateTitle" Core..=)
              Prelude.<$> incidentTemplateTitle,
            ("engagements" Core..=) Prelude.<$> engagements,
            ("incidentTemplateNotificationTargets" Core..=)
              Prelude.<$> incidentTemplateNotificationTargets,
            Prelude.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath UpdateResponsePlan where
  toPath = Prelude.const "/updateResponsePlan"

instance Core.ToQuery UpdateResponsePlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateResponsePlanResponse' smart constructor.
data UpdateResponsePlanResponse = UpdateResponsePlanResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResponsePlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateResponsePlanResponse_httpStatus' - The response's http status code.
newUpdateResponsePlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateResponsePlanResponse
newUpdateResponsePlanResponse pHttpStatus_ =
  UpdateResponsePlanResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateResponsePlanResponse_httpStatus :: Lens.Lens' UpdateResponsePlanResponse Prelude.Int
updateResponsePlanResponse_httpStatus = Lens.lens (\UpdateResponsePlanResponse' {httpStatus} -> httpStatus) (\s@UpdateResponsePlanResponse' {} a -> s {httpStatus = a} :: UpdateResponsePlanResponse)

instance Prelude.NFData UpdateResponsePlanResponse where
  rnf UpdateResponsePlanResponse' {..} =
    Prelude.rnf httpStatus
